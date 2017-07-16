
# Call libraries


library(tidyverse)
library(data.table)
library(readxl)
# Set directories
getwd()
data_directory<-paste0(getwd(),"/data")
setwd(data_directory)

# use original xlsx files - DELETE if pre-converted to csv - and convert to data.table for 
# text cleaning
data_A <- read_xlsx("Merged data_from_CHAIN_and_SQL.xlsx")
data_A <- data.table(data_A)
data_B <- read_xlsx("All Streetlink data.xlsx", sheet = 2)
data_B <- data.table(data_B)
data_C <- read_csv("StreetLink_SQL_June17.csv")
data_C <- data.table(data_C)
data_D <- read_xlsx("LocalAuthorities_matched_with_Regions.xlsx")
data_D <- data.table(data_D)
data_E <- read_xlsx("All CHAIN data.xlsx", sheet = 2)
data_E <- data.table(data_E)
# Import files (files manually converted to CSV and saved locally first)
#data_A<-fread("Merged data_from_CHAIN_and_SQL.csv")
#data_B<-fread("All StreetLink data.csv")
#data_C<-fread("StreetLink_SQL_June17.csv")
#data_D<-fread("LocalAuthorities_matched_with_Regions.csv")
#data_E<-fread("All CHAIN data.csv")

# Check dimensions of dataset
dim(data_A)
dim(data_B)
dim(data_C)
dim(data_D)
dim(data_E)

# Rename columns for easier anlaysis across different platforms people may be using
# (1) change to Title Case, (2) remove spaces and (3) remove special characters 

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

names(data_A)<-gsub("[[:punct:]]", "", gsub(" ", "",unlist(lapply(names(data_A), simpleCap))))
names(data_B)<-gsub("[[:punct:]]", "", gsub(" ", "",unlist(lapply(names(data_B), simpleCap))))
names(data_C)<-gsub("[[:punct:]]", "", gsub(" ", "",unlist(lapply(names(data_C), simpleCap))))
names(data_D)<-gsub("[[:punct:]]", "", gsub(" ", "",unlist(lapply(names(data_D), simpleCap))))
names(data_E)<-gsub("[[:punct:]]", "", gsub(" ", "",unlist(lapply(gsub("_"," ",gsub("__c","",names(data_E), ignore.case = TRUE)), simpleCap))))



# streamline local authority names for CHAIN, All Streetlink, and merged. 

# Merge A with D to get region onto the file, first clean up data_A
LAclean <- function(x) {
        
        x[,"LocalAuthority":=sapply(gsub("-"," ",LocalAuthority),simpleCap)] # Clean (A+B) local authority to sentence case and remvove "-" and .
        # clean up incorrect names that don't match the look up table
        x[,"LocalAuthority":=gsub("Stratford On Avon","Stratford Upon Avon",LocalAuthority)]
        x[,"LocalAuthority":=gsub("^Leicestershire","Leicester",LocalAuthority)]
        x[,"LocalAuthority":=gsub("Kingston Upon Hull, City Of","Kingston Upon Hull",LocalAuthority)]
        x[,"LocalAuthority":=gsub("King's Lynn And West Norfolk","King's Lynn",LocalAuthority)]
        x[,"LocalAuthority":=gsub("Herefordshire, County Of","Herefordshire",LocalAuthority)]
        x[,"LocalAuthority":=gsub("Epping Forest","Epping",LocalAuthority)]
        x[,"LocalAuthority":=gsub("East Riding Of Yorkshire","East Riding",LocalAuthority)]
        x[,"LocalAuthority":=gsub("Cannock Chase","Cannock",LocalAuthority)]
        x[,"LocalAuthority":=gsub("Blackburn With Darwen","Blackburn",LocalAuthority)]
        x[,"LocalAuthority":=gsub("Barrow In Furness","Barrow",LocalAuthority)]
        x[, "LocalAuthority" := gsub("WALTHAM FOREST", "Waltham Forest", LocalAuthority)] 

}
data_A <- LAclean(data_A)
data_B <- LAclean(data_B)
data_E <- LAclean(data_E)
names(data_D)[names(data_D)=="LAName"]<-"LocalAuthority" # rename variable in look up file to same name to prep for mergin
data_D[,"LocalAuthority":=gsub("\\.","",gsub("&","And" ,sapply(gsub("-", " ",LocalAuthority),simpleCap)))] # Replace the ampersand with "and" to match file A

## identifying overlap in tables

data_E <- data_E %>% rename(RefNo = ReferralNumber)

# Find All Streetlink records in merged with Streetlink (W) RefNo
SLinmerged <- data_B %>% semi_join(data_A, by = "RefNo")


# Flag for All Streetlink records in Merged
data_B <- data_B %>% mutate(InMerged = ifelse(is.na(RefNo), NA, ifelse(RefNo %in% SLinmerged$RefNo, 1, 0)))

# enable matching of SL(B) in CHAIN(E)
#match on number suffix only (some W not WM or WD)
data_B <- data_B %>% mutate(SLID = gsub("[[:alpha:]]", "", RefNo))
data_E <- data_E %>% mutate(SLID = gsub("[[:alpha:]]", "", StreetlinkWebsiteReferralNumber))
SLinCHAIN <- data_B %>% semi_join(data_E, by = "SLID")

# Flag for All Streetlink records sent to CHAIN 
data_B <- data_B %>% mutate(InCHAIN = ifelse(is.na(RefNo), NA, ifelse(RefNo %in% SLinCHAIN$RefNo, 1, 0))) 



# Additionally flag InMerged All Streetlink records that are in as CHAIN RefNos
CHAINinmerged <- data_E %>% semi_join(data_A, by = "RefNo")
data_B <- data_B %>% mutate(InMerged = ifelse(SLID %in% CHAINinmerged$SLID, 1, InMerged))

data_B %>% count(InMerged) # assess how many get referred

## Unify names of cols (pre initial merge)

# User details in CHAIN and Streetlink
data_E <- data_E %>% rename(UserName = ReferrerName, UserEmail = ReferrerEmail, UserTelephoneNo = ReferrerTelephone)
#  Cols in Merged data_A
data_E <- data_E %>% rename(PostDate = ReferralDate)
data_E <- data_E %>% rename(FeedbackRequested = DoesReferrerWantFeedback,
                            FeedbackProvidedToReferrer = ReferrerContactedWithOutcome, Outcome = HLOutcome,
                            DateOfFeedback = DateReferrerContacted, Channel = ReferralMethod, 
                            OutcomeNotes = ACTIONTAKENNOTES, Ethnicity = EthnicOrigin)
names(data_E)                                    
data_B <- data_B %>% rename(FeedbackRequested = ContactMeREResults)

# Cols in CHAIN and All Streetlink

data_E <- data_E %>% rename(BuildingNo = NEARESTBUILDINGNUMBER, 
                            LocationDescription = SleepingSiteDetails,
                            HowRegularly = TypicalTimeSeenAtThisSite, 
                            IssuesWeShouldBeAwareOf = ImmediateConcernsAboutClient,
                            MethodOfFeedback = HowDoesReferrerWantFeedback,
                            DateOfFollowUp = DateLAFollowUp1)
                            

## Merge files


data_merged<-merge(x=data_A, y=data_D, by = "LocalAuthority", all.x=TRUE)


data_B <- data_B %>% left_join(data_D, by = "LocalAuthority")
data_E <- data_E %>% left_join(data_D, by = "LocalAuthority")

commonColsAB <- c("RefNo", "PostDate", "LocalAuthority", "Region", "Type", "CapacityofReferral",
                "FeedbackRequested", "ReportCompleted", "FeedbackProvidedToReferrer", 
                "Outcome", "DateOfFeedback")

# Merge A with B, keeping all in A and left join to B
# Use clean data (i.e. data_A) as base file for main analyses and determine columns in B that are already in A

B_keeplist<-c("RefNo", setdiff(names(data_B),names(data_merged)))
data_Bkeeps <- data_B %>% select(one_of(B_keeplist))
#data_merged <- merge(x = data_merged, y = data_B[,mget(B_keeplist)], by = "RefNo", all.x = TRUE)

data_full <- data_merged %>% left_join(data_Bkeeps, by = "RefNo")
# add on not merged (in All Streetlink RefNo) rows of All Streetlink (data_B)
data_Bnotmerged <- data_B %>% anti_join(data_A, by = "RefNo")
data_full <- bind_rows(data_full, data_Bnotmerged)


# Create binary flag where there was no match to dataset B (i.e. the raw sql data)
#data_merged[,"NotMatchedToTableBAllFile":=ifelse(is.na(ID)==TRUE,1,0)]

# Merge (A+B) with E

E_keeplist<-c("RefNo", setdiff(names(data_E),names(data_merged)))
data_Ekeeps <- data_E %>% select(one_of(E_keeplist))
data_full <- data_full %>% left_join(data_Ekeeps, by = "RefNo")

data_Enotmerged <- data_E %>% anti_join(data_A, by = "RefNo")
data_Enotmerged <- data_Enotmerged %>% rename(ID.y = ID, LocationDescription.y = LocationDescription,
                                              TownCityOrPostcode.y = TownCityOrPostcode, BuildingNo.y = BuildingNo, 
                                              HowRegularly.y = HowRegularly, IssuesWeShouldBeAwareOf.y = IssuesWeShouldBeAwareOf,
                                              UserName.y = UserName, UserEmail.y = UserEmail, UserTelephoneNo.y = UserTelephoneNo, 
                                              DateOfFollowUp.y = DateOfFollowUp, MethodOfFeedback.y = MethodOfFeedback,
                                              ExcludeFromMergedData.y = ExcludeFromMergedData, SLID.y = SLID)
data_full <- bind_rows(data_full, data_Enotmerged)

#data_merged<-merge(x = data_merged, y = data_E[,mget(E_keeplist)], by = "RefNo", all.x = TRUE, suffixes=c("TableB","TableE"))
#names(data_merged)
#data_merged[,"NotMatchedToTableEChainFile":=ifelse(is.na(IDTableE)==TRUE,1,0)]


#### All matched now

#table(data_merged$NotMatchedToTableBAllFile, data_merged$NotMatchedToTableEChainFile) #More than half not matched to B?


## Clean data - TO BE ADDED HERE

summary(data_full)

# Write file
write.csv(data_full,"DataKind_Cleaned_Data_v3_20170711.csv",row.names=FALSE)


