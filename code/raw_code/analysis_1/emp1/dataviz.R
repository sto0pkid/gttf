# Data Viz

library(dplyr)
library(ggplot2)
library(scales)
library(viridis)

load(file = here::here("data/tidy_data",
                       "mdcs_cops_clean_df.rda"))


# MDCS counts: Histogram ---------------------------------------------------------------

# Set up palette for gttf_cop
test_pal <- viridis(8, option = "D")
test_pal[9] <- "#000000"

mdcs_cops_df %>% 
  ggplot(aes(x = date, 
             fill = gttf_cop
             )
  ) +
  theme_minimal() +
  geom_histogram(binwidth = 30,
                 alpha = 0.8) +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "6 months",
               limits = c(as.Date("1/1/2008", format = "%m/%d/%Y"), 
                          as.Date("9/1/2018", format = "%m/%d/%Y")),
               expand = c(0,0)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = test_pal) +
  scale_y_continuous(limits = c(0,NA),
                     breaks=c(50,100,150,200)) +
  labs(y = "Number of cases",
       fill = "Officer",
       caption = "Bins are 30 days wide",
       title = "Monthly District Court cases involving convicted GTTF police officers",
       subtitle = "2008 - 2018") +
  geom_vline(xintercept = as.numeric(as.Date("2017-03-10")),
             linetype=4, colour="black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-04-19")),
             linetype=4, colour="red") +
  geom_text(label = "Freddie\nGray's\nDeath", x = as.numeric(as.Date("2015-04-29")), y = 85,
            vjust = "inward", 
            hjust = "outward",
            fontface = 1,
            color = "red",
            size = 4) +
  geom_text(label = "GTTF\nOfficers\nIndicted", x = as.numeric(as.Date("2017-03-20")), y = 85,
            vjust = "inward", 
            hjust = "outward",
            fontface = 1,
            size = 4) + 
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, vjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color="black")) 

ggsave(filename = here::here("products/figures",
                             "gttf_cases_timeline.png"),
       device = "png"
       )

# MDCS counts: Flipped histogram -------------------------------------------------------


ggplot(mdcs_cops_df, aes(x = date, 
                         fill = gttf_cop)) +
  theme_minimal() +
  geom_histogram(binwidth = 30,
                 alpha = 0.8) +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "12 months",
               limits = c(as.Date("1/1/2008", format = "%m/%d/%Y"), 
                          as.Date("3/1/2019", format = "%m/%d/%Y"))
               ) +
  geom_hline(yintercept = 0) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(breaks=c(50,100,150,200)) +
  labs(y = "Number of cases",
       fill = "Cop",
       caption = "Bins are 30 days wide") +
  coord_flip() +
  geom_vline(xintercept = as.numeric(as.Date("2017-03-01")),
             linetype=4, colour="black") +
  geom_text(label = "GTTF Officers Indicted", x = as.numeric(as.Date("2017-06-01")), y = 175) + 
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) 


# MDCS counts: Histogram - separate geoms ----------------------------------------------


ggplot(mdcs_cops_df, aes(x = date)) +
  theme_minimal() +
  geom_histogram(data = mdcs_cops_df %>% filter(gttf_cop == "Hersl"),
                 binwidth = 30,
                 fill = "red") +
  geom_histogram(data = mdcs_cops_df %>% filter(gttf_cop == "Ward"),
                 binwidth = 30,
                 fill = "orange",
                 alpha = 8/9) +
  geom_histogram(data = mdcs_cops_df %>% filter(gttf_cop == "Clewell"),
                 binwidth = 30,
                 fill = "yellow",
                 alpha = 7/9) +
  geom_histogram(data = mdcs_cops_df %>% filter(gttf_cop == "Allers"),
                 binwidth = 30,
                 fill = "green",
                 alpha = 6/9) +
  geom_histogram(data = mdcs_cops_df %>% filter(gttf_cop == "Jenkins"),
                 binwidth = 30,
                 fill = "blue",
                 alpha = 5/9) +
  geom_histogram(data = mdcs_cops_df %>% filter(gttf_cop == "Hendrix"),
                 binwidth = 30,
                 fill = "blue",
                 alpha = 4/9) +
  geom_histogram(data = mdcs_cops_df %>% filter(gttf_cop == "Rayam"),
                 binwidth = 30,
                 fill = "purple",
                 alpha = 3/9) +
  geom_histogram(data = mdcs_cops_df %>% filter(gttf_cop == "Gondo"),
                 binwidth = 30,
                 fill = "pink",
                 alpha = 2/9) +
  geom_histogram(data = mdcs_cops_df %>% filter(gttf_cop == "Taylor"),
                 binwidth = 30,
                 fill = "gray",
                 alpha = 1/9) +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "12 months",
               date_minor_breaks = "6 months",
               limits = c(as.Date("1/1/2008", format = "%m/%d/%Y"), NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date",
       y = "Number of cases",
       fill = "Cop",
       caption = "Bins are 30 days wide")


# MDCS counts: Density Plot ------------------------------------------------------------


ggplot(mdcs_cops_df, aes(x = date, fill = gttf_cop, color = gttf_cop)) +
  theme_minimal() +
  geom_density(aes(y = ..count..),
               alpha = 0.1) +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "12 months",
               date_minor_breaks = "6 months",
               limits = c(as.Date("1/1/2007", format = "%m/%d/%Y"), NA))  + 
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "",
       caption = "Bins are 30 days wide")



# MDCS Demo: --------------------------------------------------------------


# Age
ggplot(mdcs_df,
       aes(x = age_yrs)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

# Race
ggplot(mdcs_df,
       aes(x = defendant_race)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()

# Sex
ggplot(mdcs_df,
       aes(x = sex_id)) +
  geom_bar() +
  theme_minimal()



# MDCS Charges ------------------------------------------------------------


mdcs_charges_df %>% count(charge_desc_2) %>% arrange(desc(n)) %>%
  ggplot(aes(x = reorder(charge_desc_2, n), y = n)) +
  geom_col(aes(fill = charge_desc_2)) +
  coord_flip() +
  labs(x = "Charge Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none")
  


# Race by Charge ----------------------------------------------------------


mdcs_charges_df %>% 
  left_join(mdcs_df) %>% 
  ggplot(aes(x = charge_desc_2, fill = defendant_race)) + 
  geom_bar() + 
  # coord_flip() + 
  theme_minimal() + 
  scale_fill_viridis(discrete = TRUE) + 
  theme(legend.position = "right",
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, vjust = 1)) + 
  labs(x = "Charge Type", y = "Count", fill = "Defendant Race") 
  

# Fill
mdcs_charges_df %>% 
  left_join(mdcs_df) %>% 
  ggplot(aes(x = charge_desc_2, fill = race_black)) + 
  geom_bar(position = "fill") + 
  coord_flip() + 
  theme_minimal() + 
  scale_fill_viridis(discrete = T) + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, vjust = 1)) + 
  labs(x = "Charge Type", y = "Proportion", fill = "Defendant Race",
       caption = "Dashed black line indicates overall proportion of nonblack race among charges\nDashed red line indicates nonblack proportion of Baltimore city residents") +
  geom_hline(yintercept = 0.14,
             linetype = 2,
             size = 1) +
  geom_hline(yintercept = 1 - 0.6374,
             linetype = 2,
             size = 1,
             color = "red") 


# Charges with sentences --------------------------------------------------


# Histogram of jailtime by race
mdcs_charges_df %>%
  filter(charge_jailtime > 0) %>%
  left_join(mdcs_df) %>%
  ggplot(aes(x = charge_jailtime, fill = race_black)) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  scale_fill_viridis(discrete = T)

# 
mdcs_charges_df %>%
  filter(charge_jailtime > 0) %>%
  left_join(mdcs_df) %>%
  group_by(race_black, charge_desc_2) %>%
  summarise(jailtime = sum(charge_jailtime)) %>%
  ggplot(aes(x = reorder(charge_desc_2, jailtime), y = jailtime, fill = race_black)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  scale_fill_viridis(discrete = T) +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(x = "Charge", y = "Total Jail Time", fill = "Race")




