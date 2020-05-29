### caseharvester EDA

library(tidyverse)
library(lubridate)

# EDA ---------------------------------------------------------------------

# dsk8_rel %>% 
#   filter(str_detect(connection,
#                     "POLICE") |
#            is.na(connection) |
#            connection == "OTHER") %>% 
#   mutate(name = toupper(name)) %>% 
#   count(name) %>% 
#   arrange(desc(n)) %>% 
#   collect() %>% 
#   View()

cases_filt %>% 
  mutate(year = year(filing_date)) %>% 
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

## Filtering to 2008-- GTTF founded in 2007
cases_filt %>% 
  mutate(year = year(filing_date)) %>% 
  filter(year >= 2008) %>% 
  # count()
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()


# plot_df <- cases_filt %>% 
#   mutate(year = year(filing_date)) %>% 
#   left_join(dscr_case_names,
#             copy = T) %>% 
#   group_by(year,
#            last_name) %>% 
#   count() %>% 
#   collect()

## Use recent data
plot_df <- case_info_df %>% 
  left_join(dscr_case_names) %>% 
  mutate(year = year(issued_date)) %>% 
  group_by(year, last_name) %>% 
  count()
  
plot_df %>% 
  mutate(count = as.numeric(n)) %>% 
  ggplot(aes(x = year,
             y = count,
             fill = last_name)) +
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = seq(min(plot_df$year), max(plot_df$year), 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Cases", fill = "")

plot_df %>% 
  filter(year >= 2000) %>% 
  mutate(count = as.numeric(n)) %>% 
  ggplot(aes(x = year,
             y = count,
             fill = last_name)) +
  geom_col() +
  facet_wrap(~last_name)+
  theme_minimal() +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = seq(min(plot_df$year)+1, max(plot_df$year), 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "", y = "Cases", fill = "")


# Charges -----------------------------------------------------------------



