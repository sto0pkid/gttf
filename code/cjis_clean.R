### kbmorales
### kbmorales@protonmail.com  
### read in MD state CJIS charge codes

library(tidyverse)
library(XML)
library(xml2)
library(readr)


# pulled from https://www.mdcourts.gov/district/chargedb
# XML format zip file

## Last revision date from website
last_revised = as.Date("2020-05-07")

### 
### CJIS codes
### 

cjis_xml <- xmlParse(here::here("data",
                                "cjiscode.xml"))

cjis_df <- xmlToDataFrame(nodes = getNodeSet(cjis_xml,
                                             "//cjischarge")) %>% 
  as_tibble()

# Clean
names(cjis_df)
glimpse(cjis_df)

cjis_df <- cjis_df %>% 
  mutate_all(as.character) %>% 
  mutate_at(c("cjisid",
              "chargeid",
              "cjiscode",
              "timeamt"),
            as.integer) %>% 
  mutate_at(c("date_begin",
              "date_end",
              "mod_date",
              "new_date"),
            as.Date) %>% 
  mutate_at(c("maxfine"), as.numeric) %>%
  ## New cols for troubleshooting
  mutate(last_revised = last_revised,
         scrape_date = Sys.Date()) %>% 
  arrange(cjisid)

###
### Traffic codes
### 

# traf_xml <- xmlParse(here::here("data",
#                                 "trafcode.xml"))

# using xml2
traf_xml <- read_xml(here::here("data",
                                "trafcode.xml")) %>% 
  xml_contents()

traf_node_names <- names(xml_attrs(traf_xml)[[1]])
for(i in 1:length(traf_node_names)) {
assign(traf_node_names[i],
       xml_attr(traf_xml, traf_node_names[i])
       )
}

base_df <- cbind(trafficid, chargeid)

traf_child_names <- xml_name(xml_children(traf_xml[[1]]))

for(i in 1:length(xml_length(traf_xml))) {
  record <- xml_attr(xml_children(traf_xml[[i]]), "value")
  names(record) <- traf_child_names
  if(i == 1){
    child_df <- record
    next
  } 
  child_df <- rbind(child_df,record)
}

traf_df <- cbind(base_df, child_df) %>% as_tibble() 

traf_df <- traf_df %>%
  mutate_at(c("trafficid",
              "chargeid"),
            as.integer) %>% 
  mutate_at(c("date_begin",
              "date_end",
              "mod_date",
              "new_date"),
            as.Date) %>% 
  mutate_at(c("fine"), as.numeric) %>% 
  mutate(last_revised = last_revised,
         scrape_date = Sys.Date())
    
### Write

last_revised <- as.character(last_revised) %>% 
  str_replace_all("-", "")

write_csv(cjis_df, here::here("data",
                              paste0("cjis_codes_", 
                                     last_revised,
                                     ".csv")
                              )
          )

write_csv(traf_df, here::here("data",
                              paste0("traf_codes_", 
                                     last_revised,
                                     ".csv")
)
)
