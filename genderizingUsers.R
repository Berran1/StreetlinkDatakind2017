# genderize based on unique name + email + phone combos
library(tidyverse)
library(readxl)
library(genderizeR)

firstNames <- read_csv("User_FirstNames.csv")
firstNames <- firstNames %>%  mutate(UserFirstName = tolower(UserFirstName))
firstNames
UserNamegender1 <- findGivenNames(firstNames$UserFirstName, textPrepare = F)
write_csv(UserNamegender1, "UserNames1.csv")
FirstNames2 <- FirstNames %>% slice(1001:n())
# for tomorrow: 
UserNamegender2 <- findGivenNames(FirstNames2$UserFirstName, textPrepare = F)
UserNamegender1
FirstNames
FirstNames3 <- FirstNames %>% slice(2002:n())
UserNamegender3 <- findGivenNames(FirstNames3$UserFirstName, textPrepare= F)
