## Flare Analysis 

rm(list = ls())

library(readxl)
library(tidyverse)
library(survminer)
library(survival)

dat <- ... %>% 
  janitor::clean_names()

dat_date <- ... %>% 
  janitor::clean_names() %>% 
  select(pid, contains("start_date"), contains("end_date")) %>% 
  select(pid, contains("nsaid")) %>% 
  select(-contains("5"))

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
  mutate_if(is.character, as.factor) %>% 
  mutate_if(~ is.numeric(.) && all(unique(.) %in% c(0, 1, NA)), factor) %>% 
  filter(
    !pid %in% exclude_list
  ) %>% 
  select(
    pid, group_nsaid_2line, contains("date")
  ) %>%
  # filter(
  #   group_nsaid_2line == "MT2"
  # ) %>% 
  select(
    pid, group_nsaid_2line,
    nsaid_1_start_date, nsaid_1_end_date, 
    nsaid_2_start_date, nsaid_2_end_date,
    nsaid_3_start_date, nsaid_3_end_date,
    nsaid_4_start_date, nsaid_4_end_date
  )

dat <- dat %>% 
  mutate(
    `Days b/w End of NSAID 1 & Start of NSAID 2` = as.Date(nsaid_2_start_date , format="%Y%m%d") - as.Date(nsaid_1_end_date, ormat="%Y%m%d"),
    `Days b/w End of NSAID 2 & Start of NSAID 3` = as.Date(nsaid_3_start_date , format="%Y%m%d") - as.Date(nsaid_2_end_date, ormat="%Y%m%d"),
    `Days b/w End of NSAID 3 & Start of NSAID 4` = as.Date(nsaid_4_start_date , format="%Y%m%d") - as.Date(nsaid_3_end_date, ormat="%Y%m%d")
  )

dat <- dat %>% 
  mutate(
    needed_nsaid2 = ifelse(`Days b/w End of NSAID 1 & Start of NSAID 2` > 0, 1, 0),
    needed_nsaid3 = ifelse(`Days b/w End of NSAID 2 & Start of NSAID 3` > 0, 1, 0),
    needed_nsaid4 = ifelse(`Days b/w End of NSAID 3 & Start of NSAID 4` > 0, 1, 0)
  )

dat_an <- dat %>% 
  janitor::clean_names() %>% 
  filter()

mod1 <- survfit(Surv(days_b_w_end_of_nsaid_1_start_of_nsaid_2, needed_nsaid2) ~ group_nsaid_2line, data = dat_an)
ggsurvplot(
  mod1, 
  type = "survival", 
  conf.int = T,
  risk.table = T,
  conf.int.alpha = 0.2, 
  cumevents = T,
  ylab = "Probability of Needing NSAID 2",
  xlab = "Days b/w End of NSAID 1 & Start of NSAID 2",
  legend.labs = c("NSAID Short", "NSAID Long", "Second Line")) 


mod2 <- survfit(Surv(days_b_w_end_of_nsaid_2_start_of_nsaid_3, needed_nsaid2) ~ group_nsaid_2line, data = dat_an)
ggsurvplot(
  mod2, 
  type = "survival", 
  conf.int = T,
  risk.table = T,
  conf.int.alpha = 0.2, 
  cumevents = T,
  ylab = "Probability of Needing NSAID 3",
  xlab = "Days b/w End of NSAID 2 & Start of NSAID 3",
  legend.labs = c("NSAID Long", "Second Line")) 


dat %>% 
  group_by(group_nsaid_2line, `Days b/w End of NSAID 1 & Start of NSAID 2`) %>% 
  drop_na(`Days b/w End of NSAID 1 & Start of NSAID 2`) %>% 
  summarize(
    n = n()
  ) %>% 
  print(n = 27)

dat %>% 
  group_by(group_nsaid_2line, `Days b/w End of NSAID 2 & Start of NSAID 3`) %>% 
  drop_na(`Days b/w End of NSAID 2 & Start of NSAID 3`) %>% 
  summarize(
    n = n()
  ) %>% 
  print(n = 27)

dat %>% 
  group_by(group_nsaid_2line, `Days b/w End of NSAID 3 & Start of NSAID 4`) %>% 
  drop_na(`Days b/w End of NSAID 3 & Start of NSAID 4`) %>% 
  summarize(
    n = n()
  )






