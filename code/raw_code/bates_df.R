# Batesdf

library(tidyverse)
library(lubridate)

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

### will come back to


# Prepare final dataset ---------------------------------------------------

bates_df %>% glimpse()

### Unsure what is needed -- write to CSV

# write_csv(bates_df, 
#           paste0("data/cases_for_bates_",
#           Sys.Date(),
#           ".csv"))
