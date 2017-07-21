# genderize based on unique name + email + phone combos
library(tidyverse)
library(readxl)
library(genderizeR)

firstNames <- read_csv("User_FirstNames.csv")
firstNames <- firstNames %>%  mutate(UserFirstName = tolower(UserFirstName))
firstNames
UserNamegender1 <- findGivenNames(firstNames$UserFirstName, textPrepare = F)

# for tomorrow: UserNamegender2 <- firstNames %>% slice(1001:n()) %>% 
# findGivenNames(UserFirstName, textPrepare = F)
UserNamegender1
