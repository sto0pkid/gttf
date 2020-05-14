### caseharvester EDA

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


plot_df <- cases_filt %>% 
  mutate(year = year(filing_date)) %>% 
  left_join(dscr_case_names,
            copy = T) %>% 
  group_by(year,
           last_name) %>% 
  count() %>% 
  collect()

plot_df %>% 
  mutate(count = as.numeric(n)) %>% 
  ggplot(aes(x = year,
             y = count,
             fill = last_name)) +
  geom_col() +
  scale_fill_viridis_d() +
  theme_minimal()


# Charges -----------------------------------------------------------------


