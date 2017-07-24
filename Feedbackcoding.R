
require(tidyverse)
#Clean Feedback Requested and FeedbackProvidedToReferrer & Method and Date of Feedback.

#data_full %>% count(FeedbackRequested)
negatives <- c("0", "false", "No", "no")
positives <- c("1", "Yes", "yes", "true")
# unify FeedbackRequested
data_full <- data_full %>% mutate(FeedbackRequested = ifelse(FeedbackRequested %in% negatives, "No", 
                                                             ifelse(FeedbackRequested %in% positives, "Yes", 
                                                                    FeedbackRequested)),
                        FeedbackRequested = ifelse(FeedbackRequested == "No Data", NA, FeedbackRequested),
                        FeedbackRequested = ifelse(FeedbackRequested == "NA - Self referred", "Self referred", FeedbackRequested),
                        FeedbackRequested.y = ifelse(FeedbackRequested.y %in% negatives, "No",
                                                     ifelse(FeedbackRequested.y %in% positives, "Yes",
                                                            FeedbackRequested.y)))


# and make all Self referrals the same
data_full <- data_full %>% mutate(FeedbackRequested = ifelse(Type == "Self", "Self referred", FeedbackRequested),
                                  FeedbackRequested.y = ifelse(Type == "Self", "Self referred", FeedbackRequested.y)) 
#data_full %>% count(FeedbackRequested.y, FeedbackRequested)
# 2984 Yes in Merged/CHAIN and No in SL. 1410 Yes in SL and no in CHAIN/Merged. significant inconsistency
# assume if Yes in one of them the user did request feedback either at first or later contact. This may be wrong 


data_full <- data_full %>% mutate(FeedbackRequested = ifelse(!is.na(FeedbackRequested) & 
                                                                              !is.na(FeedbackRequested.y) &
                                                                              FeedbackRequested != FeedbackRequested.y,
                                                                      ifelse(FeedbackRequested == "Yes" | FeedbackRequested.y == "Yes",
                                                                             "Yes", FeedbackRequested), FeedbackRequested),
                                  FeedbackRequested.y = NULL) 




# clean coding of FeedbackProvided
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(FeedbackProvidedToReferrer %in% negatives, "No",
                                                                      ifelse(FeedbackProvidedToReferrer %in% positives, "Yes", 
                                                                             FeedbackProvidedToReferrer)),
                                  FeedbackProvidedToReferrer = ifelse(FeedbackProvidedToReferrer %in% c("No requested", "Not Requested", "NA - Self referred"), 
                                                   "Not requested", FeedbackProvidedToReferrer),
                                  FeedbackProvidedToReferrer = ifelse(FeedbackProvidedToReferrer == "No Data", NA, FeedbackProvidedToReferrer))

# unite Feedback cols from CHAIN and SL
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = 
                                          ifelse(is.na(FeedbackProvidedToReferrer), FeedbackProvidedToReferrer.y,
                                                 FeedbackProvidedToReferrer))




# if Date of Feedback is provided from either source, assume feedback given. 
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(!is.na(DateOfFeedback) | !is.na(DateOfFeedback.y),
                                                         "Yes", FeedbackProvidedToReferrer))

#data_full %>% count(DateOfFeedback != DateOfFeedback.y)
# 128 where date differs. take DateofFeedback as more egregious errors in SL DateOfFeedback! 
# so differences are of < 1 month in terms of real errors. 
data_full <- data_full %>% mutate(DateOfFeedback = ifelse(is.na(DateOfFeedback), DateOfFeedback.y,
                                                          DateOfFeedback),
                                  DateOfFeedback.y = NULL) 

# if either feedback is yes in a conflict, assume someone did give feedback even if not requested.
# otherwise use CHAIN as source
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(!is.na(FeedbackProvidedToReferrer) & 
                                                                              !is.na(FeedbackProvidedToReferrer.y) &
                                                                              FeedbackProvidedToReferrer != FeedbackProvidedToReferrer.y,
                                                                      ifelse(FeedbackProvidedToReferrer == "Yes" | FeedbackProvidedToReferrer.y == "Yes",
                                                                             "Yes", FeedbackProvidedToReferrer), FeedbackProvidedToReferrer)) 


# make sure self referrals all tagged as self referred
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(Type == "Self", 
                                                                      "Self referred", FeedbackProvidedToReferrer),
                                  FeedbackProvidedToReferrer.y = ifelse(Type == "Self", 
                                                                        "Self referred", FeedbackProvidedToReferrer.y))


data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(!is.na(FeedbackRequested) & FeedbackRequested == "No" & is.na(FeedbackProvidedToReferrer),
                                                                      "Not requested", FeedbackProvidedToReferrer)) 
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer = ifelse(!is.na(FeedbackRequested) & FeedbackRequested == "No" & FeedbackProvidedToReferrer == "No",
                                                                      "Not requested", FeedbackProvidedToReferrer),
                                FeedbackProvidedToReferrer = ifelse(!is.na(FeedbackRequested) & FeedbackRequested == "Yes" & FeedbackProvidedToReferrer == "Not requested",
                                                                      "No",FeedbackProvidedToReferrer)) 




# remove other col
data_full <- data_full %>% mutate(FeedbackProvidedToReferrer.y = NULL)

# MethodOfFeedback
#data_full %>% count(MethodOfFeedback, MethodOfFeedback.y) # 4 shifted entries (dates), otherwise can merge easily
# v few overlaps, on order of 100. 19 contradictions. use MethodOfFeedback (CHAIN)
# also change name to Preferred Method Of Feedback as this was at least one of the datasets' inputs
# and may be misleading and suggest was feedback otherwise
data_full <- data_full %>% mutate(MethodOfFeedback = ifelse(is.na(MethodOfFeedback), 
                                                            MethodOfFeedback.y,
                                                            MethodOfFeedback),
                                  MethodOfFeedback = ifelse(MethodOfFeedback != "Email" & MethodOfFeedback != "Telephone",
                                                            NA, MethodOfFeedback),
                                  MethodOfFeedback.y = NULL,
                                  PreferredMethodOfFeedback = MethodOfFeedback,
                                  MethodOfFeedback = NULL)


