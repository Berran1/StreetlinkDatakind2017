# take full cleaned file and clean for just the Outcomes project - ie referrals
library(tidyverse)
library(readxl)


data_referrals <- read_csv("data/DataKind_Scrubbed_Full.csv")

# filter to those with outcomes
data_referrals <- data_referrals %>% filter(ReferredByStreetlink == 1)
names(data_referrals)
# filter out rows with Duplicate Notes, as they will double-count outcomes
# UPDATE do not ctake out duplicateNotesEntered, often different outcomes for same user so valuable.

data_referrals <- data_referrals %>% mutate(InMerged = NULL,
                                            ReferredByStreetlink = NULL) # none where both 0 and probably unhelpful
# can take out google maps URL, Lat, Lng etc because all have postcode?
#data_referrals %>% filter(is.na(Postcode)) %>% View()
#1525 of 1553  all of these are Jan 2013 or earlier - right as service was starting. except one (error?) from SL are from CHAIN AND from first 6 months of service. 
# i would say they hvaen't bothered backdating. 
# probably fair suggestion is to start at April 2013 (4 years data) if you are goign granular?

# all CHAIN data
#data_referrals %>% View()
#data_referrals %>% count(Outcome2) %>% View()
#names(data_referrals)
# keep refnos for easier back checking
data_referrals <- data_referrals %>% mutate(ID_CHAIN = NULL,
                                            StreetlinkWebsiteReferralNumber = NULL,

                                            SLID = NULL, 
                                            ID_Streetlink = NULL, 

                                            InCHAIN = NULL)
#names(data_referrals)
#data_referrals %>% count(Region)
data_referrals <- data_referrals %>% select(RowID:AppearanceTextLength, DuplicateNoteEntered, everything())
# a very initial exploration! 
#data_referrals %>% count(RegionLondon, PositiveOutcome)
data_referrals %>% write_csv("data/StreetlinkOutcomesDataset.csv")

