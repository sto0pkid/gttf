# export data as CSVs

library(googledrive)

googledrive::drive_auth()

###
## Just case numbers assocaited with cops
###

casenum_df <- cases_filt %>% 
  select(case_number) %>% 
  distinct() %>% 
  collect()

file <- paste0("data/",
               "case_numbers_indicted_",
               Sys.Date(),
               ".csv")

write_csv(casenum_df,
          file)

googledrive::drive_upload(file,
                          path = "gttf/data/")

###
## Basic cases csv
###

case_info_df <- cases_filt %>% 
  select(case_number,
         court,
         case_type,
         filing_date) %>% 
  distinct() %>% 
  left_join(dscr,
            by = "case_number") %>% 
  select(-case_type.y,
         -district_code,
         -location_code
  ) %>% 
  collect()

case_info_df

file <- paste0("data/",
               "case_info_indicted_",
               Sys.Date(),
               ".csv")

write_csv(case_info_df,
          file)

googledrive::drive_upload(file,
                          path = "gttf/data/")

###
## Basic charge info
###

charges_info_df <- dscr_chr %>% 
  filter(case_number %in% local(case_info_df$case_number)) %>%
  collect()

file <- paste0("data/",
               "charge_info_indicted_",
               Sys.Date(),
               ".csv")

write_csv(charges_info_df,
          file)

googledrive::drive_upload(file,
                          path = "gttf/data/")

###
## Officers and case numbers
### 

file <- paste0("data/",
               "cops_case_numbers_",
               Sys.Date(),
               ".csv")

write_csv(dscr_case_names,
          file)

googledrive::drive_upload(file,
                          path = "gttf/data/")

rm(file)

