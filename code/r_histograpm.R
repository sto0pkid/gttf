## Scrapes 1 mil cases a day max

library(DBI)

## Helper packages
# Dplyr
library(tidyverse)

library(dbplyr)

# Dataviz
library(ggplot2)

# Connection
source(here::here("auths","connect_ojb.R"))

### Cases - master
cases <- tbl(con, 
             in_schema("public", "cases"))

### Base R

# data has to be in R
hist(runif(100,1,100))

# have 
hist(cases$filing_date)


today <- Sys.Date()

cases %>% 
  mutate(age =local(today) - filing_date) %>% 
  select(case_number, age) %>% 
  filter(age <= 15000) %>% 
  ggplot(aes(x = age)) +
  geom_histogram()
