### Assemble dataset of cases with conspiracy charges
### kbmorales@protonmail.com

# Setup -------------------------------------------------------------------

library(tidyverse)

cjis <- read_csv("data/cjis/cjis_codes_20200507.csv")

circ_cases <- read_csv("data/circ_cases_for_bates_2020-06-24.csv")

# cleaning

circ_cases = circ_cases %>% 
  select(-id.x, -id.y)

circ_cases$cjis_traffic_code = gsub(" ", "", 
                                    circ_cases$cjis_traffic_code, 
                                    fixed = TRUE)



# Assemble conspiracy array -----------------------------------------------

glimpse(cjis)

View(cjis[str_detect(cjis$describe35, "^CON-|CONSPIRACY"),])


cjis_conspiracy= cjis$cjiscode[str_detect(cjis$describe35,
                                          "^CON-|CONSPIRACY")]

# Assemble df -------------------------------------------------------------

con_circ_cases = circ_cases %>% 
  filter(cjis_traffic_code %in% cjis_conspiracy) %>% 
  group_by(case_number) %>% 
  arrange(charge_number) %>% 
  filter(row_number() == 1)

# circ_cases %>% 
#   group_by(case_number) %>% 
#   arrange(charge_number) %>% 
#   filter(row_number() == 1)
# 
# # Compare to all circuit court criminal cases
# dsk8 %>% 
#   mutate(year = year(filing_date)) %>% 
#   filter(year >= 2000) %>% 
#   left_join(dsk8_chr, by = "case_number") %>% 
#   mutate(cjis_traffic_code = str_replace(cjis_traffic_code,
#                                          " ", ""
#                                          )) %>% 
#   filter(cjis_traffic_code %in% local(as.character(cjis_conspiracy))) %>% 
#   group_by(case_number) %>%
#   arrange(charge_number) %>%
#   filter(row_number() == 1) %>% 
#   ungroup() %>% 
#   count()
# 
# # ~2%
# 
# 
# 
# # con_circ_cases %>% glimpse()
# # 
# # con_circ_cases %>% ungroup() %>% count(verdict)
# # 
# # con_circ_cases = con_circ_cases %>% 
# #   select(colnames(dsk8)[-1])
# 


# Join cops ---------------------------------------------------------------

con_circ_cases = con_circ_cases %>% 
  left_join(dscr_case_cops, 
            by = c("district_case_number" = "case_number")) 

con_circ_cases %>% glimpse()

con_circ_cases = con_circ_cases %>% 
  select(cop = last_name,
         num_cops,
         zip_code,
         court_system,
         case_status,
         status_date,
         tracking_number,
         complaint_number,
         circuit_case_number = case_number,
         district_case_number,
         filing_date,
         incident_date
         ) %>% 
  mutate(conspiracy = TRUE)

write_csv(con_circ_cases,
          paste0("data/circ_cases_for_bates_",
                 Sys.Date(),
                 ".csv"))



# eda ---------------------------------------------------------------------

con_circ_cases %>% 
  count(cop) %>% 
  ggplot(aes(x = cop, y = n)) + 
  geom_col() + 
  coord_flip() + 
  theme_minimal() + 
  scale_x_discrete(limits = rev(c(cops$last_name, "MULTIPLE COPS"))) +
  labs(y = "# Circuit Court cases with conspiracy charges")


