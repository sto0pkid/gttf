### Assemble dataset of cases with conspiracy charges
### kbmorales@protonmail.com

# Setup -------------------------------------------------------------------

library(tidyverse)

cjis <- read_csv("data/cjis/cjis_codes_20200507.csv")

circ_cases <- read_csv("data/circ_cases_for_bates_2020-06-24.csv")

# Assemble conspiracy array -----------------------------------------------

glimpse(cjis)

cjis[str_detect(cjis$describe35, "CONSPIRACY"),]
cjis[str_detect(cjis$describe35, "CONSPIRACY"),] %>% View()

cjis_conspiracy= cjis$cjiscode[str_detect(cjis$describe35, "CONSPIRACY")]

circ_cases = circ_cases %>% 
  select(-id.x, -id.y)

circ_cases$cjis_traffic_code = gsub(" ", "", 
                                    circ_cases$cjis_traffic_code, 
                                    fixed = TRUE)

circ_cases %>% 
  filter(cjis_traffic_code %in% cjis_conspiracy) %>% 
  group_by(case_number) %>% 
  arrange(charge_number) %>% 
  filter(row_number() == 1)
