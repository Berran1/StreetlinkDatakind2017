#devtools::install_github("kalimu/genderizeR")
library(tidyverse)
library(data.table)
library(readxl)
library(digest)
library(genderizeR)



# cleaning text

# User name
names(data_full)
data_full %>% count(is.na(ID), is.na(ID.y))


# strip down to user info and record ids
user_data <- data_full %>% select(RowID, ID, ID.y, RefNo, RefNo.y, Type, PostDate, ReferrerName, ReferrerEmail, ReferrerTelephone, UserName,
                    UserEmail, UserTelephoneNo)

# Remove ReferrerName

user_data <- user_data %>% mutate(ReferrerName = NULL)

# make all names upper case (avoids sentence-case issues like O'Connor etc)
user_data <- user_data %>% mutate(UserName = toupper(UserName))


# UserEmail
# make addresses lowercase
user_data <- user_data %>% mutate(UserEmail = tolower(UserEmail),
                                  ReferrerEmail = tolower(ReferrerEmail))





# create separate phone number col to play with to find multi referrers
user_data <- user_data %>% mutate(PhoneSL = UserTelephoneNo,
                                  PhoneCHAIN = ReferrerTelephone)
# remove spaces
user_data <- user_data %>% mutate(PhoneSL = gsub(" ", "", PhoneSL),
                                  PhoneCHAIN = gsub(" ", "", PhoneCHAIN))
# help find matches for things like "ext" or "option"
user_data <- user_data %>% mutate(PhoneSL = tolower(PhoneSL),
                                  PhoneCHAIN = tolower(PhoneCHAIN))


# convert +44 to 0
user_data <- user_data %>% mutate(PhoneSL = gsub("\\+?44", "0", PhoneSL),
                                  PhoneCHAIN = gsub("\\+?44", "0", PhoneCHAIN))

# add leading 0 if 10 digit
user_data <- user_data %>% mutate(PhoneSL = gsub("(^[1-9][[:digit:]]{9}$)", "0\\1", PhoneSL),
                                  PhoneCHAIN = gsub("(^[1-9][[:digit:]]{9}$)", "0\\1", PhoneCHAIN))
# convert exponentials iff full phone number
user_data <- user_data %>% mutate(PhoneSL = gsub("(^[0-9])(\\.[0-9])([[:digit:]]{9})([eE]\\+0?11)", "0\\3", PhoneSL),
                                  PhoneCHAIN = gsub("(^[0-9])(\\.[0-9])([[:digit:]]{9})([eE]\\+0?11)", "0\\3", PhoneCHAIN)) 


# check for email contradictions
# Check for contradictions
user_data %>% filter(!is.na(ReferrerEmail), !is.na(UserEmail), 
                     ReferrerEmail != UserEmail, PhoneSL != PhoneCHAIN) %>%
        View()
# There are 75 cases where emails and phones don't match between SL and CHAIN but dates all do. 

# for purpose of original reporter, will take the Streetlink reference ones. CHAIN seems to 
# have got different info later in some cases. 
#user_data %>% filter(grepl("W", RefNo), is.na(RefNo.y)) %>% View()



user_data <- user_data %>% mutate(UserEmail = ifelse(is.na(UserEmail) & !is.na(ReferrerEmail), 
                                                               ReferrerEmail, UserEmail)) %>%
        mutate(ReferrerEmail = NULL)

# try and find "real" emails to anonymize on. replace bad emails with "0" (not NA as did enter something)

user_data <- user_data %>% 
        mutate(UserEmail = ifelse(!is.na(UserEmail) & !grepl("@", UserEmail), "0", UserEmail)) # 104 impacted



#UserTelephoneNo

user_data <- user_data %>% mutate(PhoneSL = ifelse(is.na(PhoneSL) & !is.na(PhoneCHAIN), 
                                                             PhoneCHAIN, PhoneSL)) %>%
        rename(UserPhone = PhoneSL) %>% 
        mutate(ReferrerTelephone = NULL,
               UserTelephoneNo = NULL, 
               PhoneCHAIN = NULL)


# strip out not real phone numbers. need seq of 3 digits at least because "101" is an option by the police. 
user_data <- user_data %>% mutate(UserPhone = ifelse(!is.na(UserPhone) & !grepl("[[:digit:]]{3}", UserPhone), "0", UserPhone))


# clean all data that is Self Referrer
user_data <- user_data %>% mutate(UserName = ifelse(Type == "Self", NA, UserName),
                     UserPhone = ifelse(Type == "Self", NA, UserPhone),
                     UserEmail = ifelse(Type == "Self", NA, UserEmail))
# flag for having entered a name - may correlate with outcomes. 
# Name Only relevant to SL data (not ReferrerName) as ReferrerName is all null. 
user_data <- user_data %>% 
        mutate(UserNameProvided = ifelse(!is.na(UserName), 1, ifelse(!is.na(ID.y), 0, NA)))

user_data <- user_data %>% 
        mutate(UserEmailProvided = ifelse(!is.na(UserEmail), 1, 0))

user_data <- user_data %>% mutate(UserPhoneProvided = ifelse(!is.na(UserPhone), 1, 0))

# split UserName to first and last names for better deduping and genderizing
user_data <- user_data %>% separate(UserName, into = c("UserFirstName", "UserLastName"), sep = "\\.? ",
         remove = FALSE, extra = "merge")

# extract a first name from email (via chars before punctuation) to help with gender
user_data <- user_data %>% extract(UserEmail, "EmailFirstName", "^([[:alpha:]]+)[[:punct:]]", remove = F) %>% 
        mutate(EmailFirstName = toupper(EmailFirstName))
user_data %>% filter(is.na(UserName), !is.na(EmailFirstName)) %>% count(EmailFirstName, sort = T)



# find out a first count for users
user_data %>% group_by(UserEmail, UserPhone, UserFirstName) %>% count(sort = T)
FirstNames <- user_data %>% count(UserFirstName, sort = T) #7549. can do in time :) 
write_csv(FirstNames, "User_FirstNames.csv")
user_data %>% count(EmailFirstName, sort = T) #25477 don't bother with genderizing, too messy. match on firstnames
#2085 cases of > 1 names per email (not NAs)
#3233 cases of > 1 email per name (incl first names only)
user_data %>% filter(!is.na(UserName)) %>% 
        count(UserEmail, UserPhone, wt = length(unique(UserFirstName)), sort = T) %>% filter(n >= 2) %>% View()
user_data %>% filter(!is.na(UserEmail)) %>% 
        count(UserPhone, wt = length(unique(UserEmail)), sort = T) %>% filter(n >= 2) %>% View()
#data_full %>% filter(!is.na(UserName)) %>% 
#        count(Phone, UserEmail, wt = length(unique(UserName)), sort = T) %>% View()
#

user_info <- user_data %>% count(UserEmail, UserPhone, UserFirstName, sort = T)
user_info %>% View()
user_data %>% count(UserEmail, UserPhone, sort = T)
user_data2 <- user_data #save now before real dedupling 
class(user_info)
user_data
                                                                         
                                                                          