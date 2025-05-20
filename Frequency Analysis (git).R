## Data Clean 

rm(list = ls())

library(readxl)
library(tidyverse)
library(janitor)

dat <- ... %>% 
  janitor::clean_names()

dat_date <- ... %>% 
  janitor::clean_names() %>% 
  select(pid, date_of_diagnosis) %>% 
  drop_na()

dat <- merge(
  dat, dat_date, by = "pid"
)

exclude_list <- c(37, 45, 83, 145, 12, 44, 53, 72, 128, 167,5,8,12,13,18,19,29,36,37,39,40,42,43,44,45,50,53,57,60,62,63,66,70,72,73,83,84,94,95,96,98,100,112,113,114,115,116,118,128,130,133,136,145,157,158,159,160,167,169,170,172,175,180,184,186,198,201,202,216,218,224,227,234,241,242,243,246,247)

dat <- dat %>% 
  mutate(
    Line2 = case_when(
      group_nsaid_2line == 1 ~ 0, 
      group_nsaid_2line == 2 ~ 0, 
      group_nsaid_2line == 3 ~ 1
    ),
    sex = case_when(
      female == 1 ~ "Female",
      female == 0 ~ "Male"
    ),
    group_nsaid_2line = case_when(
      group_nsaid_2line == 1 ~ "MT1",
      group_nsaid_2line == 2 ~ "MT2",
      group_nsaid_2line == 3 ~ "Second Line"
    )
  ) %>% 
  dplyr::select(
    !contains(c("mrn", "female", "race", "ethnicity"))
  ) %>% 
  drop_na() %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(~ is.numeric(.) && all(unique(.) %in% c(0, 1, NA)), factor) %>% 
  filter(
    !pid %in% exclude_list
  )

dat <- dat[, c(1, 10:14, 2:9)]

dat$year_month_of_diagnosis <- format(as.Date(dat$date_of_diagnosis), "%Y-%m")
dat$year_of_diagnosis <- format(as.Date(dat$date_of_diagnosis), "%Y")

dat_freq_sum <- dat %>% 
  group_by(
    group_nsaid_2line, year_of_diagnosis
  ) %>% 
  summarize(
    n = n()
  )


ggplot(
  data = dat_freq_sum,
  aes(
    x = year_of_diagnosis,
    y = n,
    color = group_nsaid_2line,
    group = group_nsaid_2line
  )
) + 
  geom_point(
    size = 2
  ) + 
  geom_line(
    size = 1
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  scale_y_continuous(
    breaks = c(1:12)
    ) + 
  labs(
    x = "Year of Diagnosis",
    y = "Number of Diagnoses",
    color = NULL
  )





