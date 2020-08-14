### Cops
### kbmorales
### kbmorales@protonmail.com

### From chat with Zach 2020-04-08

## Indicted or charged:
# Thomas Allers
# Keith Gladstone
# Momodu Gondo
# Robert Hankard # HANFORD is a common misspelling of HANKARD
# Evodio Hendrix
# Daniel Hersl
# Wayne Jenkins
# Craig Jester
# Ivo Louvado
# Jemell Rayam
# Victor Rivera
# Eric Snell
# Marcus Taylor
# Carmine Vignola
# Maurice Ward

## Other names of interest:
# Sherrod Biggers
# John Clewell
# Ian Dombroski
# Tariq Edwards
# Jason Giordano
# Ryan Guinn
# Kenneth Ivery
# Tariq Toro Munford
# Dean Palmere
# Michael Sylvester
# Thomas Wilson III
# Michael Woodlon

# Setup -------------------------------------------------------------------

library(tidyverse)
library(googlesheets4)

## Force no auth
gs4_deauth()
       
# First approach ----------------------------------------------------------

# Download data
gttf_cops <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1wcq2rMb0OKX2_IzMHIAM7CqIR9F4i4-NLleXERA-MLI/edit?usp=sharing"
  )

# Camille's clean names ---------------------------------------------------

cops_df = read_csv(here::here("data/cleaned_cop_names.csv"))

names_lst = str_split_fixed(cops_df$name_clean, ", | ", 3)

cops_df %>% 
  mutate(last_name = names_lst[,1],
         first_name = names_lst[,2],
         other_name = names_lst[,3]) %>% 
  filter(last_name %in% cops$last_name |
           last_name %in% cops$other_spellings) %>% 
  filter(first_name %in% cops$first_name |
           # str_detect(first_name, paste(cops$first_name, collapse = "|")) |
           str_detect(first_name,
                      # look for first initial
                      paste0("^",
                             substr(cops$first_name, 1,1),
                             "$"
                             ))) %>%
  View()
