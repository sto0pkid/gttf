library(tidyverse)
library(RPostgreSQL)
library(httr)
library(fuzzyjoin)
library(refinr)
library(stringdist)

# thought about making nickname replacements (rob --> robert) but it seems like some of those are actually different people...only make replacements if IDs are the same
# e.g. str_replace_all(name, c(!!!nicknames))
# nicknames <- tribble(
#   ~short,     ~long,
#   "ROB",      "ROBERT",
#   "DAVE",     "DAVID"
# ) %>%
#   mutate(short = sprintf("\\b%s\\b", short)) %>%
#   deframe()
abbr <- tribble(
  ~short,       ~long,
  "RBT",        "ROBERT",
  "JAM",        "JAMES",
  "JHN",        "JOHN",
  "CHA",        "CHARLES"
) %>%
    mutate(short = sprintf("\\b%s\\b", short)) %>%
    deframe()
clust_strings <- function(x, dist_method = "jaccard", q = 2, h = 0.5) {
  stringdistmatrix(x, method = dist_method, q = q) %>%
    as.dist() %>%
    hclust(method = "complete") %>%
    cutree(h = h)
}

same_length <- function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(reduce(x, identical))
  }
}

user <- rstudioapi::askForSecret("ojb_user")
pwd <- rstudioapi::askForSecret("ojb_pwd")
con <- DBI::dbConnect("PostgreSQL", dbname = "mjcs",
                      user = user,
                      password = pwd,
                      port = 5432,
                      host = "db.openjusticebaltimore.org")


# basic cleanup
# make sure space after comma to match names
# since I'm only looking at differences in names within an ID, gonna remove initials
ids <- dbGetQuery(con, "
           SELECT cases.case_number, filing_date, name, officer_id
           FROM cases
           INNER JOIN dscr_related_persons
           ON cases.case_number = dscr_related_persons.case_number
           WHERE connection IN ('WITNESS/POLICE OFFICER', 'COMPLAINANT/POLICE OFFICER')
           AND court ILIKE 'Baltimore City%'
           AND name IS NOT NULL
           AND officer_id != '0000'
           ") %>%
  as_tibble() %>%
  mutate(name = name %>%
           str_remove_all("\\b(OFFICER|OFCR?|OF+R|CPT|DET|MAJ|OFF|SGT|TPR|TRP|CPL|TFC|PFC|SPECIAL|SPEC|AGT|UNKNOWN|NOT NEEDED|NA|XX|RETIRED)\\b") %>%
           str_remove_all("[\\*\\.]") %>%
           str_remove_all("\\d") %>%
           str_replace_all(";", ",") %>%
           str_replace_all("(?<=\\b,)(\\b)", " ") %>%
           str_replace_all("\\s+", " ") %>%
           str_remove_all("(?<=[A-Z])\\s[A-Z]$") %>%
           str_replace_all(c(!!!abbr)) %>%
           trimws()) %>%
  arrange(name) %>%
  filter(str_count(name, "[A-Z]") >= 3) %>%
  distinct(name, officer_id) %>%
  rowid_to_column("id") %>%
  arrange(officer_id) %>%
  group_by(officer_id)



# not going to change IDs even if they seem wrong, so still gonna be some dupes
# instead, clustering names within each ID. 
# RULES:
# if only 1 cluster, assign name with most characters.  e.g. darren & darren q both become darren q to err on the side of too much information rather than too little
# if same length, remove extra information (usually initial). e.g. david a & david m both become david
# if more than 1 cluster, do previous rules *within* clusters
# after that, maybe some edits by hand/compare against bpd watch api
# need better handling of totally different names with 1 ID, e.g H120, I458, H648


clustered <- ids %>%
  filter(n() > 1) %>%
  nest() %>%
  mutate(cluster = map(data, pluck, "name") %>%
           map(clust_strings)) %>%
  unnest(c(data, cluster)) %>%
  mutate(nchar = nchar(name)) %>%
  group_by(officer_id)


dupes <- clustered %>%
  filter(n_distinct(cluster) == 1) %>%
  mutate(name_clean = ifelse(same_length(nchar),
                             str_remove(name, "\\s[A-Z]$"),
                             name[which.max(nchar)]))

# annoying. if there's a lot here, might be worth rechecking string distances
# focus on doing name replacement just within an ID's cluster, not for all entries with an ID
# some officers have same first name, different last name—so far only seeing this with "female" names so I'm assuming they got married. not sure what to do with that—make a rule to detect them, then stick them together?
unknown <- clustered %>%
  filter(n_distinct(cluster) > 1) %>%
  group_by(cluster, .add = TRUE) %>%
  mutate(name_clean = ifelse(same_length(nchar), name, name[which.max(nchar)]))

clean_names <- ids %>%
  filter(n() == 1) %>%
  mutate(name_clean = name) %>%
  bind_rows(dupes, unknown) %>%
  select(-cluster, -nchar, -id) %>%
  mutate(name_clean = str_remove(name_clean, ",$")) %>%
  arrange(name_clean, officer_id) %>%
  distinct(officer_id, name_clean)

write_csv(clean_names, here::here("data/cleaned_cop_names.csv"))

DBI::dbDisconnect(con)