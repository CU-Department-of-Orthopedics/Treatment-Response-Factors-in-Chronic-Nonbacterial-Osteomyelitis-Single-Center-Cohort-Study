### Bar chart

library(tidyverse)
library(readxl)

# Data 

dat_bar <- ...

# Chart 

dat_bar$Drug <-  str_wrap(dat_bar$Drug, width = 10)

ggplot(
  data = dat_bar,
  aes(
    x = reorder(Drug, - Count),
    y = Count
  )
) + 
  geom_bar(
    stat = "identity",
    fill = "grey",
    color = "black"
  ) + 
  geom_text(
    aes(
      label = Count
      ), 
    position = position_dodge(width = 0.9), vjust = -0.25
    ) + 
  ylim(0, 17) + 
  theme_bw(
    
  ) + 
  labs(
    x = "",
    y = "Count"
  )
