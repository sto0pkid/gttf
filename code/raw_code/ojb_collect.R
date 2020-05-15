### kbmorales
### kbmorales@protonmail.com
### Set up DBs from CaseHarvester


# Setup -------------------------------------------------------------------
library(tidyverse)
library(dbplyr)

options(dplyr.print_max = Inf)

# Connection
source(here::here("auths","connect_ojb.R"))

# Cops
load(here::here("data","cops_names.rda"))

## Keep just indicted for now
cops <- cops %>% 
  filter(status == "indicted")

# To Export ------------------------------------------------------------------

# Use this file

# source(here::here("code",
#                   "raw_code",
#                   "write_dfs.R"))

# Connect -------------------------------------------------------------

### Cases - master
cases <- tbl(con, 
             in_schema("public", "cases"))
# cases %>% glimpse()

## Interesting cols
# case_number
# court
# case_type
# filing_date
# status

### cc
# Circuit court civil

### dscivil
# District Court Civil

### dscr 
# District court criminal
dscr <- tbl(con, 
            in_schema("public", "dscr"))

## Sub tables

# Relations
dscr_rel <- tbl(con, 
                in_schema("public", 
                          "dscr_related_persons"))

dscr_rel %>% glimpse()

# Charges
dscr_chr <- tbl(con, 
                in_schema("public", 
                          "dscr_charges"))

dscr_chr %>% glimpse()

# Defendants

dscr_def <- tbl(con,
                in_schema("redacted",
                          "dscr_defendants"))

dscr_def %>% glimpse()

### dsk8javascript:;
# DSK8 is Circuit Court Criminal Cases

dsk8 <- tbl(con, 
            in_schema("public", "dsk8"))

dsk8 %>% glimpse()

## Sub tables
dsk8_rel <- tbl(con, in_schema("public",
                               "dsk8_related_persons"))

dsk8_rel %>% glimpse()



# Filter ---------------------------------------------------------------------

### Decided to focus on 

### Create last name filter
last_name_pat <- paste(paste0("^", cops$last_name), 
                       sep=" ",
                       collapse = "|")

### dsk8

## Look at connection values
# con_desc <- dsk8_rel %>% 
#   count(connection) %>% 
#   arrange(desc(n)) %>% 
#   collect()

dsk8_rel_filt <- dsk8_rel %>% 
  filter(str_detect(connection,
                    "POLICE") |
           is.na(connection) |
           connection == "OTHER") %>% 
  mutate(name = toupper(name)) %>% 
  filter(str_detect(name,
                    local(last_name_pat)))
  # Remove middle initials
  # mutate(name = (name,
  #                          " [A-Z]$"))
  ## Need to double check name filters
  # filter(name %in% local(cops$name))

dsk8_casenum <- dsk8_rel_filt %>% 
  select(case_number) %>% 
  collect() %>% 
  pull(case_number) %>% 
  unique()

length(dsk8_casenum)

dsk8_filt <- dsk8 %>% 
  filter(case_number %in% local(dsk8_casenum))

### dscr
# Look at connection values
con_desc <- dscr_rel %>% 
  count(connection) %>% 
  arrange(desc(n)) %>% 
  collect()

dscr_rel_filt <- dscr_rel %>% 
  filter(str_detect(connection,
                    "POLICE") |
           connection == "COMPLAINANT") %>% 
  mutate(name = toupper(name)) %>%
  filter(str_detect(name,
                    local(last_name_pat))) 
  ## Attempt to filter out the worst offenders

names_for_review <- dscr_rel_filt %>% 
  count(name) %>% 
  arrange(desc(n)) %>% 
  collect()

names_for_review$name

names_for_review %>% View()

names_test <- names_for_review %>% 
  mutate(last_name = str_extract(name,
                                 "^.+,") %>% 
           str_remove(",$"),
         first_name = str_extract(name,
                                  ", .+$") %>% 
           str_remove("^, "),
         extra_name = str_extract(first_name,
                                  " .+$") %>% 
           str_remove("^ ")
  ) %>% 
  mutate(first_name = str_remove(first_name,
                                 " .+$"))

names_test <- names_test %>% 
  filter(last_name %in% cops$last_name, 
         first_name %in% cops$first_name) %>% 
  arrange(last_name, first_name) %>% 
  filter(!(last_name == "JENKINS" & first_name == "DANIEL"),
         !(last_name == "JENKINS" & first_name == "MARCUS"),
         !(last_name == "JENKINS" & first_name == "ROBERT"),
         !(last_name == "JENKINS" & first_name == "THOMAS"),
         !(last_name == "RIVERA" & first_name == "ROBERT"),
         !(last_name == "TAYLOR" & first_name == "CRAIG"),
         !(last_name == "TAYLOR" & first_name == "DANIEL"),
         !(last_name == "TAYLOR" & first_name == "ERIC"),
         !(last_name == "TAYLOR" & first_name == "KEITH"),
         !(last_name == "TAYLOR" & first_name == "MAURICE"),
         !(last_name == "TAYLOR" & first_name == "ROBERT"),
         !(last_name == "TAYLOR" & first_name == "THOMAS"),
         !(last_name == "TAYLOR" & first_name == "VICTOR"),
         !(last_name == "WARD" & first_name == "ERIC"),
         !(last_name == "WARD" & first_name == "ROBERT"),
         !(last_name == "WARD" & first_name == "THOMAS"),
         !(last_name == "WARD" & first_name == "VICTOR"))


dscr_rel_filt_2 <- dscr_rel %>% 
  filter(str_detect(connection,
                    "POLICE") |
           connection == "COMPLAINANT") %>% 
  mutate(name = toupper(name)) %>%
  filter(name %in% local(names_test$name))


dscr_cases <- dscr_rel_filt_2 %>% 
  select(case_number, 
         name, 
         agency_code,
         agency_sub_code,
         officer_id) %>% 
  collect()

dscr_case_names <- dscr_cases %>% 
  left_join(names_test %>% select(name, last_name)) %>% 
  select(case_number, last_name)

dscr_casenum <- dscr_cases %>% 
  pull(case_number) %>% 
  unique()

length(dscr_casenum)

dscr_filt <- dscr %>% 
  filter(case_number %in% local(dscr_casenum))

### Master cases table
# casenums <- c(dsk8_casenum, dscr_casenum)
# 
# length(casenums)

cases_filt <- cases %>% 
  filter(case_number %in% local(dscr_casenum)) %>% 
  arrange(filing_date)

cases_filt %>% 
  summarise(rows = n())
