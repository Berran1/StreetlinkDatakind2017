library(forcats)
library(tidyverse)

# fill in Channel from Streetlink data
data_full <- data_full %>% mutate(Channel = ifelse(!is.na(ID.y), ifelse(grepl("WM", RefNo), 
                                                                        "Mobile", "Website"), Channel))
ChannelLevels <- na.omit(unique(data_full$Channel))

data_full <- data_full %>% mutate(Channel2 = factor(Channel, levels = ChannelLevels))
data_full <- data_full %>% mutate(Channel2 = fct_recode(Channel2,
                                                        "Website" = "Website - Streetlink", 
                                                        "Website" = "Website - NSNO",
                                                        "Telephone" = "Telephone - 0300",
                                                        "Telephone" = "Telephone - 0870"))

