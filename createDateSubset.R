# create timeseries

library(tidyverse)
install.packages("janitor")
library(janitor)


data_time <- read_csv("data/DataKind_Scrubbed_Full.csv")
date_records <- data_time %>% count(PostDate)
date_records %>% View()
# do web mobile phone and total, 
# also find some way of doing time of day. probably thru merge of date and time
data_time %>% group_by(PostDate, Channel) %>% summarise(n = n())
data_time %>% crosstab(PostDate,Channel)
data_time %>% count(Outcome)
