## Data Clean 

library(readxl)
library(tidyverse)
library(janitor)

dat <- ...

exclude_list <- c(37, 45, 83, 145, 12, 44, 53, 72, 128, 167,5,8,12,13,18,19,29,36,37,39,40,42,43,44,45,50,53,57,60,62,63,66,70,72,73,83,84,94,95,96,98,100,112,113,114,115,116,118,128,130,133,136,145,157,158,159,160,167,169,170,172,175,180,184,186,198,201,202,216,218,224,227,234,241,242,243,246,247)

dat <- dat %>% 
  clean_names() %>% 
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

dat <- dat[, c(1, 13, 2:12)]

