library(tidyverse)
library(data.table)
library(readxl)
library(forcats)
library(stringr)
library(zoo)
library(lubridate)

"%w/o%" <- function(x, y) x[!x %in% y]

 getwd()

setwd("../")
# import file 
data_full <- read_csv("data/DataKind_Merged_Data.csv")
names(data_full)

# remove the words null and N/A and replace with NA
data_full <- data_full %>% map(~ gsub("(null|N/A)", NA, .x, ignore.case = T)) %>% 
        as_tibble() 





# set date fields
datesandtimes <- c("PostDate", "PostTime","ReferralDate", "DateFirstAttempt","DateOfFollowUp", "DateLAFollowUp1",
                   "DateOfFeedback", "DateOfFeedback.y", "DateOfActiondecisionByLA", "DateCaseClosed", "ReportCompleted")
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

data_fullB <- data_full
#data_full %>% count(is.na(ReportCompleted), is.na(DateCaseClosed))
# data_full %>% select(RowID, RefNo, LocalAuthority, Outcome, one_of(datesandtimes), DateOfFeedback.y) %>% 
#         mutate(EndDifference = DateCaseClosed - ReportCompleted,
#                DecisionDifference = DateOfActiondecisionByLA - PostDate,
#                ReferralDelay = ReferralDate - PostDate,
#                FeedbackDiff = DateOfFeedback - DateOfFeedback.y,
#                AttemptDiff = DateLAFollowUp1 - parse_date(DateFirstAttempt),
#                FUDiff2 = DateOfFollowUp - PostDate,
#                ReportDiff = ReportCompleted - PostDate) %>%
#         filter(abs(ReportDiff) >300) %>%
#         View()

# 473 cases where DateOfFollowUp more than a year different from PostDate - usually because it
# is exactly a year wrong. so clearly not used, i conclude free to delete as default. 
# 2636 cases where actiondecision date != postdate; often negative by a few days. 34149 it matches
# PostDate, so that's the norm. 

#data_full %>% count(FollowedUpWithLA, LAFollowUp1) # not correlated at all
#data_full %>% count(is.na(DateOfFollowUp), FollowedUpWithLA) # some relation but not perfect
# 607 where DateFirst Attempt and DateOfFollowUp not equal and exist
#data_full %>% count(is.na(DateOfActiondecisionByLA)) #36785 entered.

#data_full %>% count(ReportCompleted != DateCaseClosed)


#data_full %>% count(!is.na(PostDate), !is.na(ReferralDate))
#data_full %>% filter(!is.na(PostDate), !is.na(ReferralDate)) %>% 
#        filter(ReferralDate - PostDate >= 1) %>% View()

# 2 cases where Post > referral, i'm calling that error. 
# 153 cases where Referral > post. much more feasible. 
# for SL load and referrals, need to take the earlier ie post. 

data_full <- data_full %>% mutate( 
               PostDate = ifelse(is.na(PostDate), ReferralDate, PostDate),
               PostDate = as.Date(PostDate, origin = "1970-01-01"),
               ReferralDate = NULL) 
#data_full <- data_full %>% arrange(PostDate, RefNo)

# 2162 cases where Report Completed before PostDate.
# some dates clearly wrong or transpositions, as in future
# one DateFirstAttempt at 2020. also the worst ReportCompleted egs that are transpositions
data_full <- data_full %>% mutate(DateFirstAttempt = gsub("2020-02-13","2013-02-20", DateFirstAttempt),
                                  ReportCompleted = gsub("2020-01-16", "2016-01-20", ReportCompleted),
                                  ReportCompleted = gsub("2018-08-18", "2015-08-18", ReportCompleted),
                                  ReportCompleted = gsub("2018-11-30", "2015-11-30", ReportCompleted)) 
                                                  
# keeping Report Completed. Clean those clearly 1 year out vs PostDate and everything else: add 1 year
data_full <- data_full %>% mutate(ReportCompleted = as.Date(ReportCompleted, origin = "1970-01-01"),
        ReportCompleted = ifelse((ReportCompleted - PostDate) < -700, 
                                        ReportCompleted %m+% years(2), ReportCompleted),
                     ReportCompleted = as.Date(ReportCompleted, origin = "1970-01-01"),
                     ReportCompleted = ifelse((ReportCompleted - PostDate) > 700,
                                              ReportCompleted %m-% years(2), ReportCompleted),
                     ReportCompleted = as.Date(ReportCompleted, origin = "1970-01-01"),
                     ReportCompleted = ifelse((ReportCompleted - PostDate) < -300,
                                              ReportCompleted %m+% years(1), ReportCompleted),
                     ReportCompleted = as.Date(ReportCompleted, origin = "1970-01-01"),
                     ReportCompleted = ifelse((ReportCompleted - PostDate) > 300,
                                              ReportCompleted %m-% years(1), ReportCompleted),
                     ReportCompleted = as.Date(ReportCompleted, origin = "1970-01-01")) 

# rename to be more clearly a date
data_full <- data_full %>% rename(ReportCompletedDate = ReportCompleted)
#data_full %>% count(DateLAFollowUp1 - DateFirstAttempt) %>% View()
#data_full %>% count(LAFollowUp1)
#251 confirmation 288 refererral being actioned 1049 response requested
# 4 NO info available rest NA

#names(data_full)
#data_full %>% count(FollowedUpWithLA, LAFollowUp1) %>% View() # only 1551 yeses which clearly not true? 
# not same results 
#data_full %>% count(CaseClosedReporting) # see Outcomescoding file
#data_full %>% count(is.na(DateOfActiondecisionByLA))
#data_full %>% filter(!is.na(RefNo.y)) %>% select(DateOfFollowUp, DateOfActiondecisionByLA,
#                                                 DateLAFollowUp1) %>% View()
#data_full %>% count(is.na(DateOfFollowUp), is.na(DateLAFollowUp1), FollowedUpWithLA)

#data_full %>% filter(!is.na(DateFirstAttempt)) %>% View()


# so keep cols PostDate, ReportCompleted, DateOfFeedback (unified)

#names(data_full)
# remove the ExcludeFromMergedData cols
#data_full %>% count(ExcludeFromMergedData) # either false or NA delete
#data_full %>% count(ExcludeFromMergedData.y) # either false or NA delete



# get rid of some more unhelpful/allNULL cols
data_full <- data_full %>% mutate(ExcludeFromMergedData = NULL,
                                  ExcludeFromMergedData.y = NULL,
                                DuplicateReport = NULL,
                                  IDNoOnly = NULL,
                                  FeedbackProvidedBy = NULL,
                                  LAEmail = NULL,
                                  ContactMeREResults = NULL,
                                  LocalAuthorityID = NULL,
                                  TermsAndConditionsId = NULL,
                                  ONGOINGACTIONBYOUTREACHTEAM = NULL,
                                  OutcomeNotes = NULL,
                                  IDNoOnly = NULL,
                                  BuildingNo = NULL,
                                  BuildingNo.y = NULL,
                                  StreetName = NULL,
                                  LatitudeLongitude = NULL)

# remove date cols we decided don't need:
data_full <- data_full %>% mutate(DateLAFollowUp1 = NULL,
                                  DateFirstAttempt = NULL,
                                  DateOfFollowUp = NULL,
                                  DateOfActiondecisionByLA = NULL)

data_fullB <- data_full #safety save

# clean outcomes
# capacity needs to be before Feedback to set type
source("StreetlinkCleaningFiles/CapacityOfReferralcoding.R")

source("StreetlinkCleaningFiles/Channelcoding.R")

source("StreetlinkCleaningFiles/OutcomesCoding.R")

source("StreetlinkCleaningFiles/Feedbackcoding.R")



data_fullB <- data_full

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



#Details about Rough Sleeper

#descriptionCols2 <- c("AgeRange", "Gender", "Ethnicity", "SkinColour", "FacialHair", "Height",
#                     "FirstName", "LastName", "Age", "RoughSleeperInfo", "Appearance")
# say that data entered if any of these cols non-NA.
#data_full <- data_full %>% 
#        mutate(RoughSleeperDataEntered = ifelse(!is.na(RoughSleeperInfo) | !is.na(Appearance) | !is.na(Age) |
#                                                           !is.na(LastName) | !is.na(FirstName) | !is.na(Height) | 
#                                                           !is.na(FacialHair) | !is.na(SkinColour) | !is.na(Ethnicity) |
#                                                           !is.na(Gender) | !is.na(AgeRange) | !is.na(SelfReferralDetails),
#                                                   1, 0)) 
# 3000 with nothing - not actually useful. so reject this idea of flag and do count of Appearance instead. 

data_full <- data_full %>% mutate(AppearanceTextLength = str_length(Appearance))
# remove all except those retained for reporting: AgeRange, Gender, Ethnicity
# these are retained for referrals only - not going to do for non-referrals
data_full <- data_full %>% mutate(SkinColour = NULL, 
                                  FacialHair = NULL,
                                  Height = NULL,
                                  FirstName = NULL,
                                  LastName = NULL,
                                  Age = NULL,
                                  RoughSleeperInfo = NULL,
                                  Appearance = NULL,
                                  SelfReferralDetails = NULL)

# recode ethnicity from CHAIN to match that in merged (cleaned) data - higher level
data_full <- data_full %>% mutate(Gender = recode(Gender, "Not known" = "Unknown"),
                                  Ethnicity = recode(Ethnicity, 
                                                     "Not sure" = "Unknown",
                                                     "Unkown" = "Unknown",
                                                     "Chinese" = "Asian",
                                                     "Arab" = "Asian"
                                  ),
                                  Ethnicity = gsub("^Asian.*", "Asian", Ethnicity),
                                  Ethnicity = gsub("^Black.*", "Black", Ethnicity),
                                  Ethnicity = gsub("^Mixed.*", "Mixed", Ethnicity),
                                  Ethnicity = gsub("^White.*", "White", Ethnicity))

# Geography

#data_full %>% filter(LocalAuthority != LocalAuthority.y) %>% View()

# 2314 where Local Authorities not the same. use CHAIN ones as more narrowed down - lots of 
# streetlink originals as "london" default location. 
regionLookup <- read_csv("data/LocalAuthorityRegions.csv")
data_full <- data_full %>% mutate(LocalAuthority = ifelse(is.na(LocalAuthority), LocalAuthority.y, LocalAuthority),
                                  LocalAuthority.y = NULL)
# join regions to used local authority
data_full <- data_full %>% left_join(regionLookup, by = "LocalAuthority")
rm(regionLookup)


#data_full %>% filter(is.na(Postcode), is.na(Lat), !is.na(LatitudeLongitude)) %>% 
#        select(RefNo, RefNo.y, Postcode, TownCityOrPostcode, TownCityOrPostcode.y, 
#               Lat, Lng, LatitudeLongitude, InMerged, InMerged.y, GoogleMapsURL) %>% View()
# can remove LatitudeLongitude as no entries where it exists but Postcode and other Lat + Lng do not
# duplicate notes
#data_full %>% filter(!is.na(RefNo.y), RefNo.y %in% DuplicateNotes) %>% select(RefNo.y:InMerged.y)%>% View()

#data_full %>% count(ONGOINGACTIONBYOUTREACHTEAM, sort = T) # only 17 T, 1863 F. probably not worth keeping
#data_full %>% count(ONGOINGACTIONBYOUTREACHTEAM, Outcome2) %>% View() # no real trend. DELETE

#data_full %>% count(OutcomeNotes, sort = T) %>% View()
# outcome notes freetext, unhelpful and mostly parallel outcomes (eg not a SS, or looked for and 
# not found, or hubbed/taken to North hub/taken to north nsno hub... Some give client names and Ref numbers/LAuth contact names. Therefore remove this field. 

# keep and rename ID cols for charity to re-use
data_full <- data_full %>% rename(ID_CHAIN = ID,
                                  ID_Streetlink = ID.y)

names(data_full)
data_fullB <- data_full

data_full <- data_full %>% mutate(Channel = NULL,
                                  Outcome = NULL) 
data_full <- data_full %>% rename(Channel = Channel2,
                                  Outcome = Outcome2)

# should we flag LAFollowUp1? 
data_full %>% count(LAFollowUp1)

data_full <- data_full %>% mutate(InMerged = ifelse(InMerged == "1" | InMerged.y == "1", 1, 0),
                     InMerged = ifelse(is.na(InMerged), 0, InMerged),
                     InMerged.y = NULL)

data_full %>% count(is.na(Postcode), FromMerged)
data_full %>% filter(is.na(Postcode), is.na(Lat)) %>% View()
# unite the UserName, Email and TelephoneNo cols

#setwd("Streetlink2017")

                                                     

write_csv(data_full, "data/DataKind_Scrubbed_Full.csv")



