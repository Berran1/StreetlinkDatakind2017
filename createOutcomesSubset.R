# take full cleaned file and clean for just the Outcomes project - ie referrals
library(tidyverse)
library(readxl)


data_referrals <- read_csv("data/DataKind_Scrubbed_Full.csv")


data_referrals <- data_referrals %>% filter(FromMerged == 1)
names(data_referrals)
# filter out rows with Duplicate Notes, as they will double-count outcomes
data_referrals <- data_referrals %>% filter(is.na(DuplicateNotes))
data_referrals <- data_referrals %>% mutate(InMerged = NULL,
                                            DuplicateNotes = NULL,
                                            FromMerged = NULL) # none where both 0 and probably unhelpful
# can take out google maps URL, Lat, Lng etc because all have postcode?
data_referrals %>% filter(is.na(Postcode)) %>% View()
#1525 of 1553  all of these are Jan 2013 or earlier - right as service was starting. except one (error?) from SL are from CHAIN AND from first 6 months of service. 
# i would say they hvaen't bothered backdating. 
# probably fair suggestion is to start at April 2013 (4 years data) if you are goign granular?
data_referrals <- data_referrals %>% mutate(Lat = NULL, 
                                            Lng = NULL,
                                            TownCityOrPostcode = NULL,
                                            TownCityOrPostcode.y = NULL, 
                                            Other = NULL,
                                            Outcome = NULL,
                                            Channel = NULL,
                                            GoogleMapsURL = NULL)
# all CHAIN data
names(data_referrals)
data_referrals %>% count(Outcome2) %>% View()
data_referrals <- data_referrals %>% mutate(DateOfFeedback = as.Date(DateOfFeedback, origin = "1970-01-01"))
summary(data_referrals$RoughSleeperDataEntered)
head(data_referrals$RoughSleeperDataEntered)
data_referrals %>% filter(!is.na(Postcode), PostDate < "2013-02-01") %>% View()
data_referrals %>% filter(FollowedUpWithLA == "Yes") %>% View()
data_referrals %>% count(RoughSleeperDataEntered == 1)
names(data_referrals)

