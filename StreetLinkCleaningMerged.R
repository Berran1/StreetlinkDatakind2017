library(tidyverse)
library(data.table)
library(readxl)
library(forcats)

getwd()
setwd("../")
data_full <- read_csv("DataKind_Cleaned_Data_20170715.csv")
names(data_full)
spec(data_full)
data_full %>% View()
# fill null as NA
data_full <- data_full %>% map(~ gsub("(null|N/A)", NA, .x, ignore.case = T)) %>% 
        as_tibble() 


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
                                  DateOfActiondecisionByLA = parse_date(DateOfActiondecisionByLA)
)  


data_full %>% count(!is.na(PostDate), !is.na(ReferralDate))
data_full %>% filter(!is.na(PostDate), !is.na(ReferralDate)) %>% 
        filter(ReferralDate - PostDate >= 1) %>% View()
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
#data_full %>% count(Type)
# change All Streetlink terminology
data_full <- data_full %>% mutate(Type = recode(Type, Report = "Referral"),
                     Type = recode(Type, `Report yourself` = "Self")) 
# change CHAIN data Type based on ReferrerType col
data_full <- data_full %>% mutate(Type = if_else(is.na(Type) & ReferrerType == "Self", "Self", Type))
data_full <- data_full %>% mutate(Type = if_else(is.na(Type) & !is.na(ReferrerType), "Referral", Type))



 


# clean outcomes to match usage report
names(data_full)
data_full %>% count(is.na(Outcome), is.na(Outcome1))
data_full %>% count(Outcome) %>% View() # unite and use CHAIN if overlap
data_full <- data_full %>% mutate(Outcome = ifelse(is.na(Outcome), Outcome.y, Outcome))
data_full <- data_full %>% mutate(Outcome = tolower(Outcome),
                                  Outcome.y = NULL,
                                  Outcome1 = NULL)
Outcomeasfactor <- na.omit(unique(data_full$Outcome))

data_full <- data_full %>% mutate(Outcome2 = factor(Outcome, levels = Outcomeasfactor))
data_full %>% count(Outcome2) %>% View()
data_full <- data_full %>% mutate(Outcome2 = fct_recode(Outcome2,
        "accommodation outcome" = "accommodation", 
        "accommodation outcome" = "action taken – housing outcome",
        "accommodation outcome" = "housing outcome",
        "person already known" = "no action taken - person already known",
        "local services did not respond" = "no action taken – local services did not respond",
        "local services did not respond" = "local services have not been able to respond to the referral", 
        "engaging with services" = "action taken – engaging with services",
        "other action taken" = "action taken – other",
        "person not found" = "no action taken - person not found",
        "outcome not yet known" = "tbc",
        "inappropriate referral" = "inappropriate referral – other",
        "inappropriate referral" = "person seeking advice - signposted to alternative services",
        "inappropriate referral" = "inappropriate referral – seeking advice",
        "other action taken" = "reconnected to services in another area",
        "other action taken" = "no action taken – no entitlement to local services" 
        ))
data_full %>% count(Outcome2)   
positiveOutcomes <- c("person already known", "accommodation outcome", "other action taken", 
                      "engaging with services", "person found - unwilling to engage")
data_full <- data_full %>% mutate(PositiveOutcome = ifelse(Outcome2 %in% positiveOutcomes, 1, 0)) 
# need to confirm these assignments! and work out what NA is. 


# get rid of some more unhelpful cols
data_full <- data_full %>% mutate(DuplicateReport = NULL,
                                  IDNoOnly = NULL,
                                  FeedbackProvidedBy = NULL,
                                  LAEmail = NULL,
                                  ContactMeREResults = NULL)

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
# match reporting from streetlink
data_full <- data_full %>% mutate(CapacityOfReferral = ifelse(CapacityOfReferral == "Healthcare professional",
                                                              "Public/Emergency Services", 
                                                              ifelse(CapacityOfReferral == "Homelessness Agency" |
                                                                             CapacityOfReferral == "Housing/Homelessness professional",
                                                                     "Homelessness Agency", CapacityOfReferral)))
data_full <- data_full %>% mutate(ReferrerType = NULL,
                                 LocalAuthorityID = NULL) 


# code binaries for filled in cols that may give too much info


data_full <- data_full %>% mutate(HowRegularlyIsEntered = 
                                          ifelse(!is.na(HowRegularly) | !is.na(HowRegularly.y), "1", "0"))
data_full <- data_full %>% mutate(HowRegularly.y = NULL) # some use of names in this so remove

descriptionCols <- c("Gender", "AgeRange", "Ethnicity", "SkinColour", "FacialHair", "Height",
                     "FirstName", "LastName", "Age", "RoughSleeperInfo", "Appearance")
data_full %>% select(one_of(descriptionCols)) %>% View()

# flag for issues aware of and then remove as quite detailed about eg mental health
data_full <- data_full %>% mutate(IssuesIsEntered = 
                                          ifelse(!is.na(IssuesWeShouldBeAwareOf) | !is.na(IssuesWeShouldBeAwareOf.y), "1", "0"))
data_full <- data_full %>% mutate(IssuesWeShouldBeAwareOf = NULL,
                                  IssuesWeShouldBeAwareOf.y = NULL)
# remove self-referral details - if not referred, doesn't help with Q2 and v privacy-breaking!
data_full <- data_full %>% mutate(SelfReferralDetails = NULL)


# unite the UserName, Email and TelephoneNo cols
getwd()
setwd("../")
source("UserDetailsCleaning.R")

write_csv(data_full, "partlycleaned170716b.csv")

