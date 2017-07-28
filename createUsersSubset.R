# take full cleaned file and clean for just the Outcomes project - ie referrals
library(tidyverse)
library(readxl)


data_users <- read_csv("data/DataKind_Scrubbed_Full.csv")

# filter out data not all of them have: postcode and wards
data_users <- data_users %>% mutate(Postcode = NULL,
                                    PostcodeNoGaps = NULL,
                                    WardGSSCode = NULL,
                                    WardLabel = NULL)

names(data_users)
#data_users %>% count(InMerged == 0, ReferredByStreetlink == 0)
#data_users %>% count(is.na(RefNoCHAIN), is.na(RefNoSL)) #19 with both NA, but can rematch on Row
# and more useful than these file ids. so keep 

#data_users %>% count(PositiveOutcome)
# remove all IDs other than ID numbers from each dataset to have re-matching even with no Referral
# ReferenceNumber
data_users <- data_users %>% mutate(ID_CHAIN = NULL,
                                            StreetlinkWebsiteReferralNumber = NULL,
                                            SLID = NULL, 
                                            ID_Streetlink = NULL, 
                                            InCHAIN = NULL,
                                    InMerged = NULL)



names(data_users)
data_users <- data_users %>% select(RowID:ReferredByStreetlink, DuplicateNoteEntered,
                                    RefNoCHAIN, RefNoSL)
data_users2 <- data_users %>% group_by(UserID) %>% mutate(a = n())
data_users2 %>% group_by(a >= 10) %>% summarise(m = mean(PositiveOutcome, na.rm = T))

data_users2 %>% View()
data_users %>% write_csv("data/StreetlinkAllReportsDataset.csv")

