#devtools::install_github("kalimu/genderizeR")
library(tidyverse)
library(data.table)
library(readxl)
library(digest)




data_full2 <- data_full


names(data_full2)

# strip down to user info and record ids
user_data <- data_full2 %>% select(RowID, ID, ID.y, Type, PostDate, ReferrerName, ReferrerEmail, ReferrerTelephone, UserName,
                    UserEmail, UserTelephoneNo)
 

# Remove ReferrerName as it is unused

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



# Check for contradictions in phone and email across CHAIN and SL sources
#user_data %>% filter(!is.na(ReferrerEmail), !is.na(UserEmail), 
#                     ReferrerEmail != UserEmail, PhoneSL != PhoneCHAIN) %>%
#        View()
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

FirstNames <- user_data %>% count(UserFirstName, sort = T) #7549. can do in time :) 
#write_csv(FirstNames, "User_FirstNames.csv")

# extract a first name from email (via chars before punctuation) to help with gender
user_data <- user_data %>% extract(UserEmail, "EmailFirstName", "^([[:alpha:]]+)[[:punct:]]", remove = F) %>% 
        mutate(EmailFirstName = toupper(EmailFirstName))
#user_data %>% count(EmailFirstName, sort = T) #25477 don't bother with genderizing, too messy. match on firstnames

# make 0 emails and phone to NA now we've done the count
user_data <- user_data %>% mutate(UserEmail = ifelse(UserEmail == 0, NA, UserEmail),
                                  UserPhone = ifelse(UserPhone == 0, NA, UserPhone))


# strip all data that is Self Referrer
#user_data <- user_data %>% filter(Type == "Referral")
## UPDATE keep in and be ok with repeat emails for id-ing repeat rough sleepers (as well as name)



# write a file for the python dedupe
#user_data %>% select(RowID, UserEmail, UserPhone, UserFirstName, UserLastName) %>% 
#        write_csv("dedupePZ/restricted2.csv")
user_dataA <- user_data #save here for exploring
#user_data <- user_dataA

# find out a first count for users
#user_data %>% group_by(UserEmail, UserPhone, UserFirstName) %>% count(sort = T)


# #2085 cases of > 1 names per email (not NAs)
# #3233 cases of > 1 email per name (incl first names only)
# user_data %>% filter(!is.na(UserName)) %>% 
#         count(UserEmail, UserPhone, wt = length(unique(UserFirstName)), sort = T) %>% filter(n >= 2) %>% View()
# user_data %>% filter(!is.na(UserEmail)) %>% 
#         count(UserPhone, wt = length(unique(UserEmail)), sort = T) %>% filter(n >= 2) %>% View()
# #data_full %>% filter(!is.na(UserName)) %>% 
# #        count(Phone, UserEmail, wt = length(unique(UserName)), sort = T) %>% View()
# #
# 
# user_data %>% count(UserEmail, UserPhone, sort = T) #66k vs 68k including first names. i'm 
# # gonna id by first names too because i'd want to know, later, if some users at a charity/centre
# were doing better so that they could set best practice. right?

# dupes <- user_data %>% get_dupes(UserEmail, UserPhone)
# dupes %>% View()
# dupeEmails <- user_data %>% get_dupes(UserEmail)
# undupes <- user_data %>% anti_join(dupes, by = "RowID")
# # find out how many have varied info
# Emailanotherphone <- undupes %>% filter(!is.na(UserEmail), UserEmail %in% dupes$UserEmail)
# Emailanotherphone %>% View()
# Phoneanotheremail <- undupes %>% filter(!is.na(UserPhone), UserPhone %in% dupes$UserPhone)
# # how many of these overlap
# bothDuped <- Emailanotherphone %>% semi_join(Phoneanotheremail) #198 # so these have nonmatched email and phone
# dupes %>% filter(UserEmail %in% bothDuped$UserEmail) %>% View()
# 
# Phoneanotheremail %>% View()
# 
# dedupeOutputs <- read_csv("dedupePZ/csv_example_output.csv")
# names(dedupeOutputs)
# dedupeOutputs <- dedupeOutputs %>% mutate(canonical_UserEmail = gsub("^b'","", canonical_UserEmail),
#                          canonical_UserEmail = gsub("'", "", canonical_UserEmail),
#                          canonical_Phone = gsub("^b'", "", canonical_Phone),
#                          canonical_Phone = gsub("'", "", canonical_Phone))
#         
# dedupes <- dedupeOutputs %>% filter(!is.na(canonical_UserEmail))
# # find ones that just matching has so far missed
# undupes %>% filter(!is.na(UserEmail), UserEmail %in% dedupes$UserEmail) %>% View()
# undupes %>% filter(grepl("emmn", UserEmail)) %>% View()
# user_data %>% count(UserEmail, wt = length(unique(UserPhone, na.rm = T))) %>% filter (n == 1)
# user_data %>% filter(UserPhone == "01895207777") %>% View()
# user_data %>% filter(UserEmail == "ta5555@hotmail.co.uk") %>% View()
# # email should beat phone (lots of switchboards!). 
# dupes %>% distinct(UserPhone) %>% View()
# dupeEmails %>% View()

#1. find useremails with only 1 unique phone number
id_emails <- user_data %>% 
        count(UserEmail, wt = length(unique(UserPhone, na.rm = T))) %>% 
        filter (n == 1)
#2. set UserID to that email (hash later!) for those phones AND phone = NA
user_data <- user_data %>% 
        mutate(UserID = ifelse(UserEmail %in% id_emails$UserEmail, UserEmail, NA))
#user_data %>% count(!is.na(UserID)) #45007 still no id. 
# 3. take out all the ones with NA Email and Phone
user_data <- user_data %>% 
        mutate(UserID = ifelse(is.na(UserEmail) & is.na(UserPhone), "Unknown", UserID))
#user_data %>% filter(is.na(UserID)) %>% View()

# left with emails with multiple phone numbers. 
#4. even if not interested in user names, use as positive ID: same user name + email is ok 
# even if phone varies. because so many phone typos or roundings or 1 digit misses!! it is unreliable. 
id_emailnames <- user_data %>% 
        filter(is.na(UserID)) %>% 
        count(UserEmail, wt = length(unique(UserFirstName, na.rm = T))) %>% filter(n == 1)
user_data <- user_data %>% mutate(UserID = ifelse(is.na(UserID) & UserEmail %in% id_emailnames$UserEmail,
                                                  UserEmail, UserID))
#user_data %>% count(is.na(UserID)) #16681
#user_data %>% filter(is.na(UserID)) %>% View()
#id_emailnames %>% View()
#5.  ok most look like email does work as id. So: do that and fill in NA gaps with phone
user_data <- user_data %>% mutate(UserID = ifelse(is.na(UserID) & !is.na(UserEmail),
                                                  UserEmail, UserID))


# now 6117 NA emails. check if known phone with single email associated
no_email <- user_data %>% filter(is.na(UserID))
#no_email %>% View()
haveIDfromEmail <- user_data %>% filter(!is.na(UserID))
phonesIDs <- haveIDfromEmail %>% select(UserPhone, UserID)
#no_email %>% semi_join(haveIDfromEmail, by = "UserPhone") %>% View() #1278 
multiemailforphone <- haveIDfromEmail %>% filter(UserPhone %in% no_email$UserPhone) %>% 
        count(UserPhone, wt = length(unique(UserEmail, na.rm = T))) %>% 
        filter(n >= 2)
uniqueemailforphone <- haveIDfromEmail %>% filter(UserPhone %in% no_email$UserPhone) %>% 
        count(UserPhone, wt = length(unique(UserEmail, na.rm = T))) %>% filter(n == 1) %>% 
        left_join(phonesIDs, by = "UserPhone") %>% distinct()
no_email2 <- uniqueemailforphone %>% right_join(no_email, by = "UserPhone") %>% 
        mutate(n = NULL, 
               UserID.y = NULL,
               UserID = UserID.x, 
               UserID.x = NULL)
#no_email2 %>% count(is.na(UserID))

# after much exploration - just use phone as ID if there's no email, for now. not perfect but fast
no_email2 <- no_email2 %>% mutate(UserID = ifelse(is.na(UserID), UserPhone, UserID))

# find phones that never have email and with common user name use phone number as ID
# phoneName <- no_email2 %>% filter(UserPhone %in% haveIDfromEmail$UserPhone == FALSE) %>% 
#         count(UserPhone, wt = length(unique(UserFirstName, na.rm = T)), sort = T) %>% 
#         filter(n == 1) 
# phoneName %>% View()
# 
# no_email2 <- no_email2 %>% 
#         mutate(UserID = ifelse(UserPhone %in% phoneName$UserPhone, UserPhone, UserID)) 
# no_email3 <- no_email2 %>% filter(!is.na(UserID))
# haveIDfromEmail <- haveIDfromEmail %>% bind_rows(no_email3)
# no_email4 <- no_email2 %>% filter(is.na(UserID)) #1407
# # multi email addresses or multi names (or no names)
# no_email4 <- no_email4 %>% 
#         mutate(UserID = ifelse(UserPhone %in% multiemailforphone$UserPhone, UserPhone, UserID)) 
# no_email4 %>% filter(is.na(UserID)) %>% View()
# user_data %>% filter(UserPhone == "02072753125") %>% View()

user_data2 <- bind_rows(haveIDfromEmail, no_email2)
# hash UserIDs that are not unknown
# and can't find tidy NSE way quickly. in future: !! so: stack overflow baseR
user_data2$UserIDhash <- sapply(user_data2$UserID, digest, algo="xxhash32") #shorter hashes than md5
user_data2 <- user_data2 %>% mutate(UserIDhash = ifelse(UserID == "Unknown", "Unknown", UserIDhash)) 
write_csv(user_data2, "data/userLookup.csv")
# create userid only for joining
######### will need to include gender here if using, before remove the names!!
user_ids <- user_data2 %>% select(RowID, UserIDhash) %>% 
        mutate(RowID = as.character(RowID)) %>%
        rename(UserID = UserIDhash)
#user_ids %>% View()
# join to data_full
data_full <- data_full %>% left_join(user_ids, by = "RowID")
names(data_full)
# remove user cols from data_full
data_full <- data_full %>% mutate(ReferrerEmail = NULL,
                                  ReferrerName = NULL,
                                  ReferrerTelephone = NULL,
                                  UserName = NULL,
                                  UserTelephoneNo = NULL,
                                  UserEmail = NULL)

#write_csv(data_full, "data/Datakind_Scrubbed_All.csv")
                                                  