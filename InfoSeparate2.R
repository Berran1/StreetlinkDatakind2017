# making one giant file 
library(tidyverse)
library(data.table)
library(readxl)
library(janitor)
library(digest)

data_details <- data_fullA

# clean up non-separated rough sleeper cols 
# extract rough sleeper col for further separation ... somehow
names(data_details)
#data_B %>% count(is.na(RoughSleeperInfo), is.na(SelfReferralDetails)) #neither entered together
#data_B %>% select(RoughSleeperInfo, SelfReferralDetails) %>% View()
# create single col for details whether self or other

RoughSleeperInfo <- data_details %>% select(RowID, RoughSleeperInfo, SelfReferralDetails, FirstName,
                                            LastName, Age, Gender, AgeRange, Ethnicity)
RoughSleeperInfo <- RoughSleeperInfo %>% 
        mutate(Joint = ifelse(!is.na(RoughSleeperInfo), RoughSleeperInfo, SelfReferralDetails))
#RoughSleeperInfo %>% View()
# split out the categories from SL
RoughSleeperInfo <- RoughSleeperInfo %>% 
        #filter(grepl("Ethnicity", Rough_Sleeper_info)) %>% 
        separate(Joint, into = c("RSI", "Ethnicityb"), sep = "Ethnicity:", extra = "merge") 

RoughSleeperInfo <- RoughSleeperInfo %>% separate(RSI, into = c("RSIb", "Ageb"), sep = "Age:", extra = "merge") 

RoughSleeperInfo <- RoughSleeperInfo %>% separate(RSIb, into = c("RSIc", "Genderb"), sep = "Gender:", extra = "merge") 

RoughSleeperInfo <- RoughSleeperInfo %>% separate(RSIc, into = c("RSId", "LastNameb"), sep = "Last name \\(if known\\):",
                                                  extra = "merge")
RoughSleeperInfo <- RoughSleeperInfo %>% separate(RSId, into = c("RSIe", "FirstNameb"), sep = "First name \\(if known\\):",
                                                  extra = "merge")  

#RoughSleeperInfo %>% 
#        count(RSIe)
# oh so there's last name and first name NOT with if known. need to split and mutate more...
RoughSleeperInfo <- RoughSleeperInfo %>% separate(RSIe, into = c("RSIf", "LastNamec"), sep = "Last name:", extra = "merge") 

RoughSleeperInfo <- RoughSleeperInfo %>% 
        mutate(LastNameb = if_else(!is.na(LastNamec), LastNamec, LastNameb)) 
RoughSleeperInfo <- RoughSleeperInfo %>% separate(RSIf, into = c("RSIg", "FirstNamec"), sep = "First name:", extra = "merge")
RoughSleeperInfo <- RoughSleeperInfo %>%
        mutate(FirstNameb = if_else(!is.na(FirstNamec), FirstNamec, FirstNameb))

RoughSleeperInfo <- RoughSleeperInfo %>% 
        mutate(FirstNamec= NULL, LastNamec = NULL)

RoughSleeperInfo <- RoughSleeperInfo %>% map(~ gsub("^[[:blank:]]+$", NA, .x)) %>% 
        as_tibble()
# still 41 outcomes not split out earlier, and blank and NA treated differently. but, ok. 
#RoughSleeperInfo %>% count(FirstNameb, LastNameb) %>% filter (n > 1) %>%
#        View()

#RoughSleeperInfo %>% filter(grepl(",", Ethnicityb) | grepl(",", Ageb) | grepl(",", Genderb)) %>% View()
#5357 rows

# check if input gender aligns


#trim spaces
RoughSleeperInfo <- RoughSleeperInfo %>% map(~ gsub("^[[:blank:]]+", "", .x)) %>% 
        as_tibble()
RoughSleeperInfo <- RoughSleeperInfo %>% map(~ gsub("Â *", "", .x)) %>% 
        as_tibble()
# extract all before comma - bad entries (5000 odd) lose some info.
#RoughSleeperInfo %>% extract(Ageb, "Agecut", "^(.+),?") %>% count(Agecut, sort = T)

RoughSleeperInfo <- RoughSleeperInfo %>% map(~ gsub(" *,.*", "", .x)) %>%
        as_tibble()
#RoughSleeperInfo %>% filter(AgeRange != Ageb) %>% View()
#str(RoughSleeperInfo)
#RoughSleeperInfo %>% crosstab(AgeRange, Ageb) %>% View()

# if AgeRange entered, keep that. If unknown or NA, use Ageb from extraction from SL. 
#RoughSleeperInfo %>% crosstab(AgeRange, Age) %>% View()
#RoughSleeperInfo %>% crosstab(Gender,Genderb) %>% View()
#RoughSleeperInfo %>% crosstab(Ethnicity, Ethnicityb) %>% View()
RoughSleeperInfo <- RoughSleeperInfo %>% mutate(Ageb = gsub("25-50", "25 - 50", Ageb),
                                                Ageb = gsub("none", "Unknown", Ageb))
RoughSleeperInfo <- RoughSleeperInfo %>% mutate(Ethnicityb = gsub(" .*", "", Ethnicityb))
RoughSleeperInfo <- RoughSleeperInfo %>% 
        mutate(AgeRange = ifelse(AgeRange == "Unknown" | is.na(AgeRange),
                                 Ageb,
                                 AgeRange),
               Gender = ifelse(Gender == "Unknown", 
                               Genderb, 
                               Gender),
               Ethnicity = ifelse(Ethnicity == "Unknown" | Ethnicity == "Unkown" | is.na(Ethnicity),
                                  Ethnicityb,
                                  Ethnicity))

## names
RoughSleeperInfo <- RoughSleeperInfo %>% mutate(FirstName = toupper(FirstName), 
                                                FirstNameb = toupper(FirstNameb),
                                                LastName = toupper(LastName),
                                                LastNameb = toupper(LastNameb))
#RoughSleeperInfo %>% filter(FirstName != FirstNameb) %>% View()
#RoughSleeperInfo %>% count(is.na(FirstName), is.na(FirstNameb)) #14363 both entered 3914 differ
# assume CHAIN (FirstName) has more details, based on experience with service.
RoughSleeperInfo <- RoughSleeperInfo %>% mutate(FirstName = ifelse(is.na(FirstName), 
                                                                   FirstNameb,
                                                                   FirstName),
                                                FirstNameb = NULL,
                                                LastName = ifelse(is.na(LastName),
                                                                  LastNameb,
                                                                  LastName),
                                                LastNameb = NULL)
#RoughSleeperInfo %>% View()
names(RoughSleeperInfo)
# remove cols that have been used
RoughSleeperInfo <- RoughSleeperInfo %>% mutate(`RoughSleeperInfo` = NULL,
                                                SelfReferralDetails = NULL,
                                                Age = NULL, 
                                                Genderb = NULL,
                                                Ageb = NULL,
                                                Ethnicityb = NULL,
                                                RSIg = NULL)

#trim spaces
RoughSleeperInfo <- RoughSleeperInfo %>% map(~ gsub("^[[:blank:]]+", "", .x)) %>% 
        as_tibble()
RoughSleeperInfo <- RoughSleeperInfo %>% 
        mutate(AgeRange = gsub(".*40\\+", "Unknown", AgeRange),
               AgeRange = gsub("^[[:space:]]+", "Unknown", AgeRange),
               AgeRange = ifelse(RowID == "100496", "Unknown", AgeRange),
               Gender = gsub("none|Not known", "Unknown", Gender),
               Ethnicity = ifelse(RowID == "16860", "Unknown", Ethnicity),
               Ethnicity = recode(Ethnicity, 
                                              "Not sure" = "Unknown",
                                              "Not" = "Unknown",
                                              "none" = "Unknown",
                                              "Unkown" = "Unknown",
                                              "Chinese" = "Asian",
                                              "Arab" = "Asian"
               ),
               Ethnicity = gsub("^Asian.*", "Asian", Ethnicity),
               Ethnicity = gsub("^Black.*", "Black", Ethnicity),
               Ethnicity = gsub("^Mixed.*", "Mixed", Ethnicity),
               Ethnicity = gsub("^White.*", "White", Ethnicity),
               Ethnicity = gsub("[[:blank:]]+", NA, Ethnicity))

#give up and find row by hand!
#RoughSleeperInfo %>% filter(!is.na(AgeRange)) %>% View()
#str(RoughSleeperInfo$AgeRange)

# join to data_full by RowID and nullify originals
data_full <- data_full %>% left_join(RoughSleeperInfo, by = "RowID") %>%
        mutate(RoughSleeperGender = Gender.y,
               Gender.x = NULL,
               Gender.y = NULL,
               RoughSleeperAgeRange = AgeRange.y, 
               AgeRange.x = NULL,
               AgeRange.y = NULL,
               RoughSleeperEthnicity = Ethnicity.y,
               Ethnicity.x = NULL,
               Ethnicity.y = NULL,
               RoughSleeperFirstName = FirstName.y,
               FirstName.x = NULL,
               FirstName.y = NULL,
               RoughSleeperLastName = LastName.y,
               LastName.x = NULL,
               LastName.y = NULL
               )
# clean names and flag for after anonymizing
data_full <- data_full %>% 
        mutate(RoughSleeperFirstName = gsub("NA|UNKNOWN|NOT KNOWN|N/?K|\\?|[[:blank:]]+|-|TEST", NA, RoughSleeperFirstName),
                     RoughSleeperLastName = gsub("NA|UNKNOWN|NOT KNOWN|N/?K|\\?|[[:blank:]]+|-|TEST", NA, RoughSleeperLastName),
               RSFirstNameEntered = ifelse(!is.na(RoughSleeperFirstName), 1, 0),
               RSLastNameEntered = ifelse(!is.na(RoughSleeperLastName), 1, 0))
#data_full %>% count(is.na(RoughSleeperFirstName))

# find out if should hash just on RS name or email too
#user_data %>% filter(Type == "Self") %>% count(is.na(RoughSleeperLastName), 
#                                               (is.na(UserEmail) | is.na(UserPhone)))
#14k firstname but no email or phone, 6k at least one of them, 31 email or phone only, 641 NA both
# but may want to id same RS if self referred or not. so just do name, iff firstname lastname
# 945 both NA for last name. so don't id on first name only basically
data_full <- data_full %>% mutate(RoughSleeperName = ifelse(!is.na(RoughSleeperFirstName) &
                                                                    !is.na(RoughSleeperLastName),
                                                            paste0(RoughSleeperFirstName, RoughSleeperLastName),
                                                            "Unknown"))
data_full$RoughSleeperID <- sapply(data_full$RoughSleeperName, digest, algo="xxhash32") #shorter hashes than md5
data_full <- data_full %>% mutate(RoughSleeperID = ifelse(RoughSleeperName == "Unknown", "Unknown", RoughSleeperID)) 

#write file that has the names in
data_full %>% select(RowID, RoughSleeperFirstName, RoughSleeperLastName, RoughSleeperName,
                     RoughSleeperID) %>% write_csv("data/RoughSleeperNameLookup.csv")
# remove Identifying info and just leave hashes
data_full <- data_full %>% mutate(RoughSleeperFirstName = NULL,
                                  RoughSleeperLastName = NULL,
                                  RoughSleeperName = NULL)

#data_full %>% filter(RoughSleeperID != "Unknown") %>% count(RoughSleeperID, sort = T) %>% filter(n >= 2)
#data_full %>% filter(RoughSleeperID == "59d62a78") %>% View()

#data_full %>% View()
               