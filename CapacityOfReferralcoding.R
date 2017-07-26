

library(tidyverse)

# generate Type for all rows (Referral or Self)
#str(data_full$Type)
#
# change All Streetlink terminology
data_full <- data_full %>% 
        mutate(Type = ifelse(Type == "Report", "Referral", 
                                                ifelse(Type == "Report yourself", "Self", Type)))

# change CHAIN data Type based on ReferrerType col
data_full <- data_full %>% mutate(Type = if_else(is.na(Type) & ReferrerType == "Self", "Self", Type))
data_full <- data_full %>% mutate(Type = if_else(is.na(Type) & !is.na(ReferrerType), "Referral", Type))
# leaves 2 as NA

#data_full %>% count(Type, CapacityOfReferral) %>% View()

# if Type is Self but Capacity is Unknown, Self just in case for the privacy
data_full <- data_full %>% mutate(
                    CapacityOfReferral = ifelse(Type == "Self", "Self", CapacityOfReferral),
                    ReferrerType = ifelse(Type == "Self", "Self", ReferrerType))

# Clean CapacityOfReferral and Other
#data_full %>% count(CapacityOfReferral)
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
#data_full %>% count(CapacityOfReferral)
#data_full %>% filter(CapacityOfReferral == "Unknown" | is.na(CapacityOfReferral)) %>% count(Other, sort = T) %>% View()
data_full %>% count(CapacityOfReferral,Other) %>% filter(grepl("neighb", Other, ignore.case= T)) %>% View()
#data_full %>% filter(!is.na(Other),CapacityOfReferral == "Unknown") %>% count(Other, sort = T)
## User OTHER (from Streetlink data) to code more user options for unknown and NA

# all member of public and variations Unknown and NA coded to  members of public; other is more complex so leave for now 
data_full <- data_full %>% 
        mutate(CapacityOfReferral = ifelse((CapacityOfReferral == "Unknown" | 
                                                    is.na(CapacityOfReferral)) & 
                                                   grepl("member.*public|resident|just|concerned", Other, ignore.case = T),
                                           "Member of the Public", CapacityOfReferral))

# all passer by change to Member of the Public too. 
data_full <- data_full %>% 
        mutate(CapacityOfReferral = ifelse((CapacityOfReferral == "Unknown" |
                                                    CapacityOfReferral == "Other" |
                                                    is.na(CapacityOfReferral)) & 
                                                   grepl("[Pp]ass", Other) &
                                                   grepl("[Bb]y", Other),
                                           "Member of the Public", 
                                           CapacityOfReferral))
# # all friend or freind codings in Other NA and most in Unknown should be Friend or Family.
# tidy up the unknown ones that don't fit this first
data_full <- data_full %>%
        mutate(CapacityOfReferral = ifelse((CapacityOfReferral == "Unknown" |
                                                    is.na(CapacityOfReferral)) & 
                                                   grepl("chair|safety", Other, ignore.case = T),
                                           "Member of the Public", 
                                           CapacityOfReferral),
               CapacityOfReferral = ifelse((CapacityOfReferral == "Unknown" |
                                                    CapacityOfReferral == "Other" |
                                                    is.na(CapacityOfReferral)) & 
                                                   grepl("friend|freind", Other, ignore.case = T),
                                           "Friend or Family", 
                                           CapacityOfReferral))

# NOTE could read these Others and hand-code by line essentially to a known category, but not highest
# priority. 
# Note also that some say self or give personal info because people typed into it as freetext so Other 
# needs to be removed. 

# so if something entered in Other and is Unknown or NA in Capacity, i think we call that Other. 
# if NANA or Unknown NA then do Unknown. this should at least remove one category! 
# enough people had tech issues with dropdown that not truly different entry motivations necessarily
# then make Other NULL

data_full <- data_full %>% 
        mutate(CapacityOfReferral = ifelse(!is.na(Other) & 
                                                   (CapacityOfReferral == "Unknown" | 
                                                            is.na(CapacityOfReferral)), 
                                           "Other",
                                           CapacityOfReferral),
               CapacityOfReferral = ifelse(is.na(CapacityOfReferral), 
                                           "Unknown", CapacityOfReferral),
               Other = NULL)

