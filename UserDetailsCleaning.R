devtools::install_github("kalimu/genderizeR")
library(tidyverse)
library(data.table)
library(readxl)
library(forcats)

# cleaning text

# User name
names(data_full)
# Remove ReferrerName
data_full %>% count(ReferrerName)
data_full <- data_full %>% mutate(ReferrerName = NULL)

# make all names upper case (avoids sentence-case issues like O'Connor etc)
data_full <- data_full %>% mutate(UserName = toupper(UserName))


# UserEmail
# make addresses lowercase
data_full <- data_full %>% mutate(UserEmail = tolower(UserEmail),
                                  ReferrerEmail = tolower(ReferrerEmail))





# remove spaces in phone numbers

# create separate phone number col to play with to find multi referrers
data_full <- data_full %>% mutate(PhoneSL = UserTelephoneNo,
                                  PhoneCHAIN = ReferrerTelephone)
# remove spaces
data_full <- data_full %>% mutate(PhoneSL = gsub(" ", "", PhoneSL),
                                  PhoneCHAIN = gsub(" ", "", PhoneCHAIN))
# convert +44 to 0
data_full <- data_full %>% mutate(PhoneSL = gsub("\\+?44", "0", PhoneSL),
                                  PhoneCHAIN = gsub("\\+?44", "0", PhoneCHAIN))

# add leading 0 if 10 digit
data_full <- data_full %>% mutate(PhoneSL = gsub("(^[1-9][[:digit:]]{9}$)", "0\\1", PhoneSL),
                                  PhoneCHAIN = gsub("(^[1-9][[:digit:]]{9}$)", "0\\1", PhoneCHAIN))
# convert exponentials iff full phone number
data_full <- data_full %>% mutate(PhoneSL = gsub("(^[0-9])(\\.[0-9])([[:digit:]]{9})([eE]\\+0?11)", "0\\3", PhoneSL),
                                  PhoneCHAIN = gsub("(^[0-9])(\\.[0-9])([[:digit:]]{9})([eE]\\+0?11)", "0\\3", PhoneCHAIN)) 


# check for email contradictions
# Check for contradictions
data_full %>% filter(!is.na(ReferrerEmail), !is.na(UserEmail), 
                     ReferrerEmail != UserEmail, PhoneSL != PhoneCHAIN) %>% 
        select(PostDate, ReferralDate, ReferrerEmail, PhoneCHAIN, 
               UserName, UserEmail, PhoneSL, LocationDescription, LocationDescription.y, 
               LocalAuthority, LocalAuthority.y) %>%
        
        View()
# There are 75 cases where emails and phones don't match but dates all do, locations seem similar. 


data_full %>% filter(!is.na(ReferrerTelephone), !is.na(UserTelephoneNo),
                     PhoneSL != PhoneCHAIN, ReferrerEmail != UserEmail) %>% 
        select(PostDate, ReferralDate, ReferrerEmail, PhoneCHAIN, 
               UserName, UserEmail, PhoneSL, LocationDescription, LocationDescription.y, 
               LocalAuthority, LocalAuthority.y) %>% 
        View()
# 1242 with non matching phone; 75 with non matching phone and email
# for purpose of original reporter, will take the Streetlink reference ones. 


# go back to standard naming
#data_full <- data_full %>% mutate(UserTelephoneNo = Phone) %>% 
        #mutate(Phone = NULL)

data_full <- data_full %>% mutate(UserEmail = ifelse(is.na(UserEmail) & !is.na(ReferrerEmail), 
                                                               ReferrerEmail, UserEmail)) %>%
        mutate(ReferrerEmail = NULL)



#UserTelephoneNo

data_full <- data_full %>% mutate(PhoneSL = ifelse(is.na(PhoneSL) & !is.na(PhoneCHAIN), 
                                                             PhoneCHAIN, PhoneSL)) %>%
        rename(Phone = PhoneSL) %>% 
        mutate(ReferrerTelephone = NULL,
               UserTelephoneNo = NULL, 
               PhoneCHAIN = NULL)
# help find matches for things like "ext" or "option"
data_full <- data_full %>% mutate(Phone = tolower(Phone))
# strip out not real phone numbers. 3 because "101" is an option by the police. 
data_full <- data_full %>% mutate(Phone = ifelse(grepl("[[:digit:]]{3}", Phone), Phone, NA))

# find out a first count for users
#data_full %>% group_by(UserEmail, Phone, UserName) %>% count(sort = T)

#2085 cases of > 1 names per email (not NAs)
#3233 cases of > 1 email per name (incl first names only)
#data_full %>% filter(!is.na(UserEmail)) %>% 
#        count(UserName, wt = length(unique(UserEmail)), sort = T) %>% View()
#data_full %>% filter(!is.na(UserEmail)) %>% 
#        count(Phone, wt = length(unique(UserEmail)), sort = T) %>% View()
#data_full %>% filter(!is.na(UserName)) %>% 
#        count(Phone, UserEmail, wt = length(unique(UserName)), sort = T) %>% View()
#
