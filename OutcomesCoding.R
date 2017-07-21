
library(tidyverse)
library(forcats)



# clean outcomes to match usage report
names(data_full)
#data_full %>% count(Outcome) %>% View()
#data_full %>% count(is.na(Outcome), is.na(Outcome.y))
#data_full %>% count(Outcome, Outcome1) %>% View()

data_full <- data_full2
#data_full %>% count(Outcome) %>% View() # unite and use CHAIN (Outcome) if overlap
# because presumably it's better recorded. 
# remove Outcome1 and then the other SL outcomes
data_full <- data_full %>% mutate(Outcome = ifelse(is.na(Outcome), Outcome.y, Outcome))
data_full <- data_full %>% mutate(Outcome = tolower(Outcome),
                                  Outcome.y = NULL,
                                  Outcome1 = NULL)
Outcomeasfactor <- na.omit(unique(data_full$Outcome))

data_full <- data_full %>% mutate(Outcome2 = factor(Outcome, levels = Outcomeasfactor))
data_full %>% filter(Outcome2 == "created on chain") %>% View()
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
                                                        "other action taken" = "person seeking advice - signposted to alternative services",
                                                        "inappropriate referral" = "inappropriate referral – seeking advice",
                                                        "other action taken" = "reconnected to services in another area",
                                                        "inappropriate referral" = "no action taken – no entitlement to local services" 
)) 
# to check: created on chain  but no CHAIN reference (n = 367)

positiveOutcomes <- c("person already known", "accommodation outcome", "other action taken", 
                      "engaging with services", "person found - unwilling to engage")
data_full <- data_full %>% mutate(PositiveOutcome = ifelse(Outcome2 %in% positiveOutcomes, 1, 0)) 
# need to confirm these assignments! and work out what NA is. 
data_full %>% count(Outcome2) %>% write_csv("Outcomes.csv")

data_full2 <- data_full
