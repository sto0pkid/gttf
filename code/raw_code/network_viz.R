### Network viz
### kbmorales@protonmail.com
### kbmorales

# TODO: 
# Brandon Soderberg re: GTTF-affiliated cops and connection levels 
# (i.e., 10, 20, 50, etc.). His response:
# "For now I’d say limit it to 25 or so. I’ll do some more thinking but one 
# thing is to try and locate it with partners. For example, Gondo and Rayam often
# have a lot of cases together. Or Jenkins and Frieman. The interesting or useful 
# factor might be to see who shows up along with that combo (as just two examples) 
# rather than who shows up with an individual. If that makes sense."

# Setup -------------------------------------------------------------------

library(tidygraph)
library(ggraph)
library(tidyverse)

## Run bates_df.R first

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
