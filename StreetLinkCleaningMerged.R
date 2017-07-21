library(tidyverse)
library(data.table)
library(readxl)
library(forcats)
library(stringr)

# getwd()
# setwd("../")
# import file 
data_full <- read_csv("data/DataKind_Merged_Data.csv")
# add unique id to get back
data_full <- data_full %>% rowid_to_column(var = "RowID")
data_full <- data_full %>% map(~ gsub("(null|N/A)", NA, .x, ignore.case = T)) %>% 
        as_tibble() 

summary(data_full)


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

# set date fields
data_full <- data_full %>% mutate(PostDate = parse_date(PostDate),
                                  ReferralDate = parse_date(ReferralDate),
                                  ReportCompleted = parse_date(ReportCompleted),
                                  DateOfFeedback = parse_date(DateOfFeedback),
                                  DateLAFollowUp1 = parse_date(DateLAFollowUp1),
                                  DateFirstAttempt = parse_date(DateFirstAttempt),
                                  DateCaseClosed = parse_date(DateCaseClosed),
                                  DateOfFollowUp = parse_date(DateOfFollowUp),
                                  DateOfFeedback.y = parse_date(DateOfFeedback.y),
                                  DateOfActiondecisionByLA = parse_date(DateOfActiondecisionByLA),
                                  PostTime = parse_time(PostTime)
)  


#data_full %>% count(!is.na(PostDate), !is.na(ReferralDate))
#data_full %>% filter(!is.na(PostDate), !is.na(ReferralDate)) %>% 
#        filter(ReferralDate - PostDate >= 1) %>% View()
# 2 cases where Post > referral, i'm calling that error. 
# 153 cases where Referral > post. much more feasible. 
# for SL load and referrals, need to take the earlier ie post. 
# try to unite to postdate but lose formatting. < 300 records so ignore. 
data_full <- data_full %>% mutate(ReferralDelay = ReferralDate - PostDate) #%>% 
#        mutate(PostDate2 = as.date(ifelse(is.na(PostDate), ReferralDate, PostDate))) %>%
#        mutate(ReferralDate = NULL)  

data_full <- data_full %>% arrange(PostDate, RefNo)
                                                  


#names(data_full)
# remove the ExcludeFromMergedData cols
#data_full %>% count(ExcludeFromMergedData) # either false or NA delete
#data_full %>% count(ExcludeFromMergedData.y) # either false or NA delete
data_full <- data_full %>% mutate(ExcludeFromMergedData = NULL, ExcludeFromMergedData.y = NULL)

# generate Type for all rows
#str(data_full$Type)
#data_full %>% count(ReferrerType)
# change All Streetlink terminology
data_full <- data_full %>% mutate(Type = recode(Type, Report = "Referral"),
                     Type = recode(Type, `Report yourself` = "Self")) 
# change CHAIN data Type based on ReferrerType col
data_full <- data_full %>% mutate(Type = if_else(is.na(Type) & ReferrerType == "Self", "Self", Type))
data_full <- data_full %>% mutate(Type = if_else(is.na(Type) & !is.na(ReferrerType), "Referral", Type))



# get rid of some more unhelpful cols
data_full <- data_full %>% mutate(DuplicateReport = NULL,
                                  IDNoOnly = NULL,
                                  FeedbackProvidedBy = NULL,
                                  LAEmail = NULL,
                                  ContactMeREResults = NULL,
                                  LocalAuthorityID = NULL)


data_full2 <- data_full #safety save

# clean outcomes
source("OutcomesCoding.R")

#Clean Feedback Requested and FeedbackProvidedToReferrer
data_full %>% count(FeedbackRequested)
negatives <- c("0", "false", "No", "no")
positives <- c("1", "Yes", "yes", "true")
data_full <- data_full %>% mutate(FeedbackRequested = ifelse(FeedbackRequested %in% negatives, "No", 
                                                ifelse(FeedbackRequested %in% positives, "Yes", 
                                                       FeedbackRequested))) %>% 
        mutate(FeedbackRequested = ifelse(FeedbackRequested == "No Data", NA, FeedbackRequested)) %>%
        mutate(FeedbackRequested = ifelse(FeedbackRequested == "NA - Self referred", "Self referred", FeedbackRequested))


data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(FeedbackProvidedToReferrer %in% negatives, "No",
                                                         ifelse(FeedbackProvidedToReferrer %in% positives, "Yes", 
                                                                FeedbackProvidedToReferrer))) %>% 
        mutate(FeedbackProvidedToReferrer = ifelse(FeedbackProvidedToReferrer %in% c("No requested", "Not Requested", "NA - Self referred"), 
                                                   "Not requested", FeedbackProvidedToReferrer)) %>% 
        mutate(FeedbackProvidedToReferrer = ifelse(FeedbackProvidedToReferrer == "No Data", NA, FeedbackProvidedToReferrer))

# make sure self referrals all tagged as not requested
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(FeedbackRequested == "Self referred", 
                                                         "Not requested", FeedbackProvidedToReferrer),
                     FeedbackProvidedToReferrer.y = ifelse(FeedbackRequested == "Self referred", 
                                                           "Not requested", FeedbackProvidedToReferrer.y))

                     
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = 
                             ifelse(is.na(FeedbackProvidedToReferrer), FeedbackProvidedToReferrer.y,
                                  FeedbackProvidedToReferrer))

# if either feedback is yes in a conflict, assume someone did give feedback! otherwise use CHAIN
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(!is.na(FeedbackProvidedToReferrer) & 
                                                                 !is.na(FeedbackProvidedToReferrer.y) &
                                                                 FeedbackProvidedToReferrer != FeedbackProvidedToReferrer.y,
                                                         ifelse(FeedbackProvidedToReferrer == "Yes" | FeedbackProvidedToReferrer.y == "Yes",
                                                                "Yes", FeedbackProvidedToReferrer), FeedbackProvidedToReferrer)) 
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(FeedbackRequested == "No" & FeedbackProvidedToReferrer == "No",
                                                                      "Not requested", FeedbackProvidedToReferrer))
# remove other col
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer.y = NULL)
data_full2 <- data_full



# Clean CapacityOfReferral
data_full %>% count(CapacityOfReferral)
data_full <- data_full %>% mutate(CapacityOfReferral = ifelse(is.na(CapacityOfReferral), ReferrerType, 
                                                             CapacityOfReferral))
data_full <- data_full %>% mutate(CapacityOfReferral= ifelse(CapacityOfReferral == "Member of the public",
                                                             "Member of the Public", 
                                                             ifelse(CapacityOfReferral == "Other (please specify)",
                                                                    "Other", 
                                                                    ifelse(CapacityOfReferral == "Friend or family",
                                                                           "Friend or Family",
                                                                           ifelse(CapacityOfReferral == "Police officer",
                                                                                  "Public/Emergency Services", 
                                                                                  CapacityOfReferral))))) 
# don't match reporting from streetlink: Gareth said that homelessness agency vs housing professional 
# maybe on their way home may be something they want to look at and keep as seprate categories. 
# data_full <- data_full %>% mutate(CapacityOfReferral = ifelse(CapacityOfReferral == "Healthcare professional",
#                                                               "Public/Emergency Services", 
#                                                               ifelse(CapacityOfReferral == "Homelessness Agency" |
#                                                                              CapacityOfReferral == "Housing/Homelessness professional",
#                                                                      "Homelessness Agency", CapacityOfReferral)))
data_full <- data_full %>% mutate(ReferrerType = NULL) 

data_full %>% count(CapacityOfReferral, Other, sort = T) %>% View()
# code binaries for filled in cols that may give too much info SUPERSEDED
# code length of text for cols that may reveal info.

# How Regularly
# for the clashes, use the Streetlink (.y) data: that is where most data entered and 
# CHAIN is more limited to time of day cutout NOT the whole text. so CHAIN is confounder for lenghth.
data_full <- data_full %>% 
        mutate(HowRegularly.y = ifelse(is.na(HowRegularly.y), HowRegularly, HowRegularly.y),
               HowRegularly = HowRegularly.y,
               HowRegularly.y = NULL)

data_full <- data_full %>% mutate(HowRegularlyTextLength = str_length(HowRegularly),
                                  HowRegularly = NULL)



# Issues aware of (remove but report length as proxy for detail - can use names and be v detailed about eg mental health)
#data_full %>% filter(!is.na(IssuesWeShouldBeAwareOf), !is.na(IssuesWeShouldBeAwareOf.y)) %>% View()
# Streetlink has fuller data and more filled in. use that length if clash. 
data_full <- data_full %>% 
        mutate(IssuesWeShouldBeAwareOf.y = ifelse(is.na(IssuesWeShouldBeAwareOf.y),
                                                  IssuesWeShouldBeAwareOf, IssuesWeShouldBeAwareOf.y),
               IssuesWeShouldBeAwareOf = IssuesWeShouldBeAwareOf.y,
               IssuesWeShouldBeAwareOf.y = NULL)
data_full <- data_full %>% mutate(IssuesWeShouldBeAwareOfTextLength = str_length(IssuesWeShouldBeAwareOf),
                     IssuesWeShouldBeAwareOf = NULL)

# also location description - has identifiable stuff, and Streetlink original entry (ie y)
# should be used if a conflict (more entered and more overlap here, 24610)

data_full <- data_full %>% 
        mutate(LocationDescription.y = ifelse(is.na(LocationDescription.y),
                                                  LocationDescription, LocationDescription.y),
               LocationDescription = LocationDescription.y,
               LocationDescription.y = NULL)
data_full <- data_full %>% mutate(LocationDescriptionTextLength = str_length(LocationDescription),
                                  LocationDescription = NULL)

# remove self-referral details - obviously PID
data_full <- data_full %>% mutate(SelfReferralDetails = NULL)

#Details about Rough Sleeper

descriptionCols2 <- c("AgeRange", "Gender", "Ethnicity", "SkinColour", "FacialHair", "Height",
                     "FirstName", "LastName", "Age", "RoughSleeperInfo", "Appearance")
# say that data entered if any of these cols non-NA.
data_full <- data_full %>% 
        mutate(RoughSleeperDataEntered = ifelse(!is.na(RoughSleeperInfo) | !is.na(Appearance) | !is.na(Age) |
                                                           !is.na(LastName) | !is.na(FirstName) | !is.na(Height) | 
                                                           !is.na(FacialHair) | !is.na(SkinColour) | !is.na(Ethnicity) |
                                                           !is.na(Gender) | !is.na(AgeRange),
                                                   1, 0)) 
# remove all except those retained for reporting: AgeRange, Gender, Ethnicity
data_full <- data_full %>% mutate(SkinColour = NULL, 
                                  FacialHair = NULL,
                                  Height = NULL,
                                  FirstName = NULL,
                                  LastName = NULL,
                                  Age = NULL,
                                  RoughSleeperInfo = NULL,
                                  Appearance = NULL)

data_full2 <- data_full

names(data_full)
# Geography
# remove maps URL as privacy-breaching
data_full <- data_full %>% mutate(GoogleMapsURL = NULL)

# unite the UserName, Email and TelephoneNo cols
getwd()
setwd("../")
source("UserDetailsCleaning.R")

write_csv(data_full, "partlycleaned170716b.csv")

