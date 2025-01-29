# import excel data

library(readxl)
library(tidyverse)

data = read_excel("W02b_homeprice.xlsx")
sorted.top.unaval = data %>%
  arrange(desc(data$unaval)) %>%
  slice(1:10)

barplot(
  height = sorted.top.unaval$unaval,
  names = sorted.top.unaval$`MSA (Metropolitan Statistical Area)`,
  
)