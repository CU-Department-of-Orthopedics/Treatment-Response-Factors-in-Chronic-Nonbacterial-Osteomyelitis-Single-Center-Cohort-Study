## Secondary Analysis (Legion Location)

## Data Clean 

rm(list = ls())

library(readxl)
library(tidyverse)
library(janitor)

dat2 <- ...

dat2 <- dat2 %>% 
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
  mutate_if(~ is.numeric(.) && all(unique(.) %in% c(0, 1, NA)), factor)

dat2 <- dat2[, c(8:13, 15, 18)]

names(dat2)

dat2 <- dat2 %>% 
  mutate(
    location = case_when(
      head_face == 1 ~ "Head/Face",
      upper_torso == 1 ~ "Upper Torso",
      upper_extremity == 1 ~ "Upper Ext.",
      lower_extremity == 1 ~ "Lower Ext.",
      neck_back == 1 ~ "Neck/Back",
      lower_torso == 1 ~ "Lower Torso"
    )
  ) %>% 
  mutate(
    location = case_when(
      regions_affected > 1 ~ "Multiple",
      TRUE ~ location
    )
  )

dat2 <- dat2[, 8:9]

table(dat2$group_nsaid_2line, dat2$location)

stats::chisq.test(table(dat2$group_nsaid_2line, dat2$location), simulate.p.value = T)

## Plots 

ggplot(
  data = dat2, 
  aes(
    x = location,
    fill = group_nsaid_2line
  )
) + 
  geom_bar(
    stat = "count",  width = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)
  ) + 
  labs(
    fill = "Trt. Group",
    x = "Lesion Location",
    y = "Count"
  )
