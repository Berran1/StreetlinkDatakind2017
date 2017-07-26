# create timeseries

library(tidyverse)

library(janitor)
library(lubridate)


data_date <- read_csv("data/DataKind_Scrubbed_All.csv")
# remove NA dates
data_date <- data_date %>% filter(!is.na(PostDate))
# do web mobile phone and total, 
# also find some way of doing time of day. probably thru merge of date and time

datesByChannel <- data_date %>% filter(!is.na(Channel)) %>% crosstab(PostDate,Channel) %>% 
        select(PostDate, Website, Mobile, Telephone) %>% # put web and mobile together - same reporting
        mutate(Total = Mobile + Telephone + Website)

# create days with 0 
days<-tibble(PostDate=seq(as.Date(min(data_date$PostDate, na.rm = T)),
                             as.Date(max(data_date$PostDate, na.rm = T)),"day"))


datesByChannel <- datesByChannel %>% full_join(days) %>% 
        arrange(PostDate) %>%
        map( ~ ifelse(is.na(.x),0,.x)) %>%
        as_tibble() %>% 
        mutate(PostDate = as.Date(PostDate, origin = "1970-01-01"))

write_csv(datesByChannel, "data/Reports_by_Date.csv")

data_date$PostTime
# create date time for web and mobile
datetimeReports <- data_date %>% 
        filter(!is.na(PostTime)) %>%
        mutate(PostDateTime = ymd_hms(paste(PostDate, PostTime))) %>% 
        select(PostDateTime, Channel, everything())
datetimeReports %>% group_by(PostDateTime) %>% mutate(Count = n()) %>% 
        select(PostDate, PostTime, PostDateTime, Count) %>% arrange(PostDateTime) %>% View()
datetimeReports %>% count(PostDate, PostTime, PostDateTime)
datetimeReports %>% count(PostTime)
