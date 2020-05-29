# Batesdf

library(tidyverse)
library(tidygraph)
library(ggraph)

## Filter 1: indicted cops (done elsewhere)

## Filter 2: year 2000 on
bates_df = case_info_df %>% 
  # May dupe cases
  # left_join(dscr_case_names) %>% 
  mutate(year = year(issued_date)) %>% 
  filter(year >= 2000)

## Filter 3: 
# need to go into charges dzta
bates_chr_df <- dscr_chr %>% 
  filter(case_number %in% local(bates_df$case_number)) %>%
  collect()

bates_chr_df %>% 
  pull(charge_description) %>% 
  unique()

bates_chr_df %>% 
  count(statute_description) %>% 
  arrange(desc(n)) %>% 
  View()

## Use filter from analysis 1
bates_chr_df <- bates_chr_df %>% 
  mutate(charge_description_2 = case_when(
  statute == "CR.5.601.(a)(1)" & 
    !str_detect(charge_description, "NOT") | 
    str_detect(charge_description, "POSSESSION - MARI") |
    str_detect(charge_description, "CDS: POSS[-\\s]MARI[HJ]UANA") |
    str_detect(charge_description, "CON-CDS:POSS-MARIHUANA") ~ 
    "Marijuana Possession",
  statute == "CR.5.601.(a)(1)" & 
    str_detect(charge_description, "NOT|POSSESS-NOT") |
    str_detect(charge_description, "POSSESS-NOT") |
    str_detect(charge_description, "CDS-UNLAWFUL POSSESSION ETC") |
    str_detect(charge_description, "CDS POSSESSION-CON") |
    str_detect(charge_description, "CDS:POSSESSION") |
    str_detect(charge_description, "CDS POSSESS - LG AMT") ~ 
    "Non-Marijuana Possession",
  str_detect(statute, "CR.5.602") | 
    str_detect(charge_description, "DIST") |
    str_detect(charge_description, "NARC POSS W/") |
    str_detect(charge_description, "CDS[-\\s]POSS W/I") |
    str_detect(charge_description, "CDS-POSS WI MANUF/DIS/DISP-NARC-CON")	~ 
    "CDS Distribituion / Manufacture",
  str_detect(charge_description, "PARA") ~ "CDS Paraphernalia Possession",
  str_detect(charge_description, "FIREARM|RFL|PISTOL|RIFLE|GUN|AMMO") ~ 
    "Firearms-related",
  str_detect(charge_description, "ASSAULT") |
    str_detect(charge_description, "DANGEROUS WEAPON-INT/INJURE") ~
    "Assault",
  str_detect(charge_description, "ARREST") ~ "Resisting Arrest",
  str_detect(charge_description, "TRESPASS") ~ "Trespassing",
  str_detect(charge_description, "THEFT|ROBB|BURG|	CARJACKING") ~ 
    "Theft / Burglary / Robbery",
  str_detect(charge_description, "DISORDERLY CONDUCT") ~ "Disorderly Conduct",
  str_detect(charge_description, "VIOLATION OF PROB") ~ "Probation Violation",
  TRUE ~ "Other"
)
)

bates_chr_df %>% 
  count(charge_description_2) %>% 
  ggplot(aes(x = reorder(charge_description_2,n),
             y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(x = "",
       y = "Count") +
  theme_minimal()
  

### Look for gun and drug charges
bates_chr_casenum <- bates_chr_df %>% 
  filter(charge_description_2 %in% c("CDS Distribituion / Manufacture",
                                     "CDS Paraphernalia Possession",
                                     "Firearms-related",
                                     "Marijuana Possession",
                                     "Non-Marijuana Possession")) %>% 
  pull(case_number) %>% 
  unique()

bates_df <- bates_df %>% 
  filter(case_number %in% bates_chr_casenum)


# filter 4 ----------------------------------------------------------------

# related persons associated with these:

bates_df$case_number

dscr_case_names %>% count(new_name)

test_net <- dscr_rel %>% 
  filter(case_number %in% local(bates_df$case_number)) %>% 
  filter(str_detect(connection,
                    "POLICE") |
           connection == "COMPLAINANT") %>% 
  filter(!is.na(name)) %>% 
  select(name, case_number) %>% 
  distinct() %>% 
  collect()

## Try keeping last name and first initial
test_net <- test_net %>% 
  mutate(name = str_extract(name, "^\\w+, \\w"))
  
test_net <- test_net %>% left_join(dscr_case_names %>% rename(cop_name = name)) %>% 
  select(-last_name)

## Remvove cops connecting to themselves
test_net <- test_net %>% 
  filter(!name %in% unique(dscr_case_names$name))

# save to revet
revert_net <- test_net
test_net <- revert_net

###TEST 
# Filter to n > 10
# filt_df <- test_net %>% 
#   group_by(cop_name) %>% 
#   count(name) %>% 
#   filter(n > 10)
# 
# test_net <- test_net %>% 
#   semi_join(filt_df)

###TEST 2
# Filter to n > 20
filt_df <- test_net %>% 
  group_by(cop_name) %>% 
  count(name) %>% 
  filter(n > 50)

test_net <- test_net %>% 
  semi_join(filt_df)

# Node df -----------------------------------------------------------------
sources <- test_net %>%
  count(cop_name) %>%
  rename(label = cop_name)

destinations <- test_net %>%
  count(name) %>%
  rename(label = name)

nodes <- full_join(sources, destinations, by = "label") %>% 
  rowid_to_column("id") %>%
  mutate(n = case_when(
    !is.na(n.x) ~ n.x,
    TRUE ~ n.y
  )) %>%
  select(-n.x, -n.y)

connects <- test_net %>%  
  group_by(cop_name, name) %>%
  summarise(weight = n()) %>% 
  ## Try filtering weights
  # filter(weight >= 10) %>%
  ungroup() %>%
  arrange(desc(weight))

## Remove small weight nodes
# nodes <- nodes %>% 
#   filter(label %in% connects$cop_name | label %in% connects$name)

edges <- connects %>% 
  left_join(nodes, by = c("cop_name" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("name" = "label")) %>% 
  rename(to = id)

edges <- edges %>% select(from, to, weight, cop_name)

### TEST
# edges <- edges %>% filter(weight >= 10)
# nodes <- nodes %>% filter(id %in% unique(c(edges$from, edges$to)))

test <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

# 1
ggraph(test) + 
  geom_edge_density(aes(fill = cop_name)) + 
  geom_edge_link(aes(alpha = weight),
                 show.legend = F) +
  # geom_edge_link(aes(width = weight,
  #                    color = cop_name), 
  #                alpha = 0.6) + 
  geom_node_point(aes(size = n),
                  alpha = 0.8,
                  show.legend = F) + 
  geom_node_text(aes(label = label),
                 color = "gray30",
                 check_overlap = T,
                 repel = T) +
  theme_graph()

test %>% 
  ggraph(layout = "kk") + 
  geom_edge_fan(aes(color = cop_name),
                alpha = 0.4) + 
  geom_node_point(aes(size = n),
                  alpha = 1) +
  # geom_node_text(aes(label = label),
  #                size = 2,
  #                repel = TRUE) +
  # scale_edge_color_viridis(discrete = TRUE) +
  theme_graph() +
  geom_node_text(aes(label = label)) +
  labs(title = "GTTF Officers Network Map",
       subtitle = "Cases tried after the death of Freddie Gray",
       edge_colour = "GTTF Officer",
       size = "Connections") +
  theme(legend.position = "bottom")

ggsave(filename = here::here("products/figures",
                             "gttf_network.png"),
       device = "png"
)

ggraph(test, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Cases") +
  theme_graph()

# ggraph(test, layout = 'dendrogram', circular = TRUE) + 
#   geom_edge_diagonal(color = gttf_cop) + 
#   geom_node_point() + 
#   coord_fixed()
