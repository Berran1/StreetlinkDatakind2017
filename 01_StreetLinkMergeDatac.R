
# Call libraries


library(tidyverse)
library(data.table)
library(readxl)
library(forcats)
# Set directories
getwd()
data_directory<-paste0(getwd(),"/data")
setwd(data_directory)

# use original xlsx files - DELETE if pre-converted to csv - and convert to data.table for 
# text cleaning
data_A <- read_xlsx("Merged data_from_CHAIN_and_SQL.xlsx")
# add identifier for these records
data_A <- data_A %>% mutate(FromMerged = 1)
str(data_A)
# convert to data table for the local authority cleaning
data_A <- data.table(data_A)



data_B <- read_xlsx("All Streetlink data.xlsx", sheet = 2)
data_B <- data.table(data_B)
str(data_B)


#data_C <- read_csv("StreetLink_SQL_June17.csv")
#data_C <- data.table(data_C)
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

# add post time from sql file to All Streetlink data and then remove sql file - not otherwise needed
sqldata <- read_csv("StreetLink_SQL_June17.csv")

#separate sql data Post date_1 col into date and time
sqldata <- sqldata %>% separate(`Post date_1`, into = c("PostDate1", "PostTime"), sep = " - ") 

#sqldata %>% count(is.na(PostTime)) # no times are missing
#sqldata %>% count(`Post date` == PostDate1) # all dates match up

# subset for joining
recordTimes <- sqldata %>% select(`Ref no`, PostTime)

# add PostTime to data_B and insert next to Post date
data_B <- data_B %>% left_join(recordTimes, by = "Ref no") %>% 
        select(ID:`Post date`, PostTime, everything())
#data_B %>% count(is.na(PostTime)) #241 have no time recorded - v low proportion, acceptable
data_B <- data.table(data_B)
# remove sql data
rm(sqldata)


# Rename columns for easier anlaysis across different platforms people may be using
# (1) change to Title Case, (2) remove spaces and (3) remove special characters 

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

names(data_A)<-gsub("[[:punct:]]", "", gsub(" ", "",unlist(lapply(names(data_A), simpleCap))))
names(data_B)<-gsub("[[:punct:]]", "", gsub(" ", "",unlist(lapply(names(data_B), simpleCap))))
#names(data_C)<-gsub("[[:punct:]]", "", gsub(" ", "",unlist(lapply(names(data_C), simpleCap))))
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

## identifying overlap in tables and setting common names with Merged where simple

data_E <- data_E %>% rename(RefNo = ReferralNumber)

# add a duplicate refno column to both E and A so that they can be used more easily
# after merging with merged.
data_E <- data_E %>% mutate(RefNoCHAIN = RefNo)
data_B <- data_B %>% mutate(RefNoSL = RefNo)

data_E <- data_E %>% rename(FeedbackRequested = DoesReferrerWantFeedback,
                            FeedbackProvidedToReferrer = ReferrerContactedWithOutcome, 
                            Outcome = HLOutcome,
                            DateOfFeedback = DateReferrerContacted, Channel = ReferralMethod, 
                            OutcomeNotes = ACTIONTAKENNOTES, Ethnicity = EthnicOrigin)

data_B <- data_B %>% rename(FeedbackRequested = ContactMeREResults)

# Cols in CHAIN and All Streetlink

data_E <- data_E %>% rename(BuildingNo = NEARESTBUILDINGNUMBER, 
                            LocationDescription = SleepingSiteDetails,
                            HowRegularly = TypicalTimeSeenAtThisSite, 
                            IssuesWeShouldBeAwareOf = ImmediateConcernsAboutClient,
                            MethodOfFeedback = HowDoesReferrerWantFeedback
)


# Find All Streetlink records in merged with Streetlink (W) RefNo
SLinmerged <- data_B %>% semi_join(data_A, by = "RefNo")
# Flag for All Streetlink records in Merged
data_B <- data_B %>% mutate(InMerged = ifelse(is.na(RefNo), NA, ifelse(RefNo %in% SLinmerged$RefNo, 1, 0)))

# Flag InMerged All CHAIN records in merged
CHAINinmerged <- data_E %>% semi_join(data_A, by = "RefNo")
data_E <- data_E %>% mutate(InMerged = ifelse(is.na(RefNo), NA, ifelse(RefNo %in% CHAINinmerged$RefNo, 1, 0))) 

# enable matching of SL(B) in CHAIN(E)

#data_E %>% filter(!is.na(StreetlinkWebsiteReferralNumber), !grepl("^W", StreetlinkWebsiteReferralNumber)) %>% 
#        View()
# 182 entries that don't start with W. Most misaligned ie addresses. 
# 1 of these has lowercase. 
data_E <- data_E %>% mutate(StreetlinkWebsiteReferralNumber = gsub("wd", "WD", StreetlinkWebsiteReferralNumber))
#data_E %>% count(grepl("W", StreetlinkWebsiteReferralNumber),
#                 grepl("WM", StreetlinkWebsiteReferralNumber), 
#                 grepl("WD", StreetlinkWebsiteReferralNumber)) %>% 
#        View()
# 1630 W only, 16737 WD, 6388 WM. 

#match on just numbers of streetlinkreferral for CHAIN (some W not WM or WD)
data_B <- data_B %>% mutate(SLID = as.character(gsub("[[:alpha:]]", "", RefNo)))
data_E <- data_E %>% mutate(SLID = as.character(gsub("W[MD]?", "", StreetlinkWebsiteReferralNumber))) 
SLinCHAIN <- data_B %>% semi_join(data_E, by = "SLID")

# Flag for All Streetlink records sent to CHAIN 
data_B <- data_B %>% mutate(InCHAIN = ifelse(is.na(RefNo), NA, ifelse(RefNo %in% SLinCHAIN$RefNo, 1, 0))) 


# create a way to look across the rows of same-SLID data
CHAINwithSL <- data_E %>% filter(grepl("^W", StreetlinkWebsiteReferralNumber)) %>% left_join(SLinCHAIN, by = "SLID") 
CHAINwithSL %>% select(HowRegularly.y) %>% View()
# write to file for later deduping
write_csv(CHAINwithSL, "CHAINwithSL.csv")
write_csv(data_D, "LocalAuthorityRegions.csv")
             

## Merge files


#data_merged <- data_A %>% left_join(data_D, by = "LocalAuthority")
data_merged <- data_A # join regions after resolving LA conflicts and cleaning - next file!
#data_B <- data_B %>% left_join(data_D, by = "LocalAuthority")
#data_E <- data_E %>% left_join(data_D, by = "LocalAuthority")
#CHAINwithSL <- CHAINwithSL %>% left_join(data_D, by = "LocalAuthority")

# Merge A with CHAINwithSL (to get all cols)
# remove .x from CHAIN names to allow easier merging
names(CHAINwithSL) <- gsub("\\.x", "", names(CHAINwithSL))

CHAINwithSLkeeplist <- c("RefNo", setdiff(names(CHAINwithSL), names(data_merged)))
CHAINwithSLkeeps <- CHAINwithSL %>% select(one_of(CHAINwithSLkeeplist))

data_CSL <- data_merged %>% inner_join(CHAINwithSLkeeps, by = "RefNo")


# Merge data_merged with rest of data_E 
data_Eother <- data_E %>% anti_join(CHAINwithSL, by = "RefNo")
Eother_keeplist<-c("RefNo", setdiff(names(data_Eother),names(data_merged)))
data_Eotherkeeps <- data_Eother %>% select(one_of(Eother_keeplist))


data_ME <- data_merged %>% inner_join(data_Eotherkeeps, by = "RefNo")
# create Merged + CHAIN - CHAIN data added to all CHAIN refs in Merged
data_MC <- bind_rows(data_CSL, data_ME)



# Merge A with B that weren't in the join with CHAIN, keeping all in A and left join to B

names(data_MC) %>% subset(grepl("\\.y", names(data_MC)))
# add suffixes to data_B names for binding with data_merged
data_Bother <- data_B %>% anti_join(CHAINwithSL, by = c("ID" = "ID.y"))
data_Bother <- data_Bother %>% rename(ID.y = ID, RefNo.y = RefNo, LocalAuthority.y = LocalAuthority, 
                       BuildingNo.y = BuildingNo, TownCityOrPostcode.y = TownCityOrPostcode,
                       LocationDescription.y = LocationDescription, HowRegularly.y = HowRegularly,
                       IssuesWeShouldBeAwareOf.y = IssuesWeShouldBeAwareOf,
                       DateOfFeedback.y = DateOfFeedback, MethodOfFeedback.y = MethodOfFeedback,
                       FeedbackProvidedToReferrer.y = FeedbackProvidedToReferrer,
                       Outcome.y = Outcome, ExcludeFromMergedData.y = ExcludeFromMergedData,
                       InMerged.y = InMerged)
Bother_keeplist<-c("RefNo.y", setdiff(names(data_Bother),names(data_merged)))
data_Botherkeeps <- data_Bother %>% select(one_of(Bother_keeplist))

# dataset of Merged with Streetlink references (and not duplicated in CHAIN) with Streetlink cols
data_MB <- data_merged %>% inner_join(data_Botherkeeps, by = c("RefNo" = "RefNo.y"))

# create Merged with CHAIN from SL and CHAIN not with SL and SL not with CHAIN ref
# this would (after further cleaning) be basis for "outcomes" question
data_MCS <- bind_rows(data_MC, data_MB)

# bind rows not referred (ie not in Merged file originally) from each source

CHAINwithSLnotmerged <- CHAINwithSL %>% anti_join(data_merged, by = "RefNo")
data_all <- bind_rows(data_MCS, CHAINwithSLnotmerged)

data_Eothernotmerged <- data_Eother %>% anti_join(data_merged, by = "RefNo")
data_all <- bind_rows(data_all, data_Eothernotmerged)
# add on not merged (in All Streetlink RefNo) rows of All Streetlink (data_B)

data_Bothernotmerged <- data_Bother %>% anti_join(data_merged, by = c("RefNo.y" = "RefNo"))
data_all <- bind_rows(data_all, data_Bothernotmerged)


# add RowIDs. 
data_all <- data_all %>% rowid_to_column(var = "RowID")


# Write file
write.csv(data_all,"DataKind_Merged_Data.csv",row.names=FALSE)

# preserve entries that are duplicates in merged (with Streetlink and CHAIN entries) n = 125
missingmerged <- data_A %>% anti_join(data_MCS, by = "RefNo") 

dupsB <- data_B %>% filter(RefNo %in% missingmerged$RefNo)
dupsB <- dupsB %>% rename(ID.y = ID, RefNo.y = RefNo, LocalAuthority.y = LocalAuthority, 
                          BuildingNo.y = BuildingNo, TownCityOrPostcode.y = TownCityOrPostcode,
                          LocationDescription.y = LocationDescription, HowRegularly.y = HowRegularly,
                          IssuesWeShouldBeAwareOf.y = IssuesWeShouldBeAwareOf,
                          DateOfFeedback.y = DateOfFeedback, MethodOfFeedback.y = MethodOfFeedback,
                          FeedbackProvidedToReferrer.y = FeedbackProvidedToReferrer,
                          Outcome.y = Outcome, ExcludeFromMergedData.y = ExcludeFromMergedData,
                          InMerged.y = InMerged)
dupsB_keeplist<-c("RefNo.y", setdiff(names(dupsB),names(data_merged)))
dupsBkeeps <- dupsB %>% select(one_of(dupsB_keeplist))
#data_merged <- merge(x = data_merged, y = data_B[,mget(B_keeplist)], by = "RefNo", all.x = TRUE)

duplicatesinMerged <- data_merged %>% inner_join(dupsBkeeps, by = c("RefNo" = "RefNo.y"))
# save the list of double entries just in case
write_csv(duplicatesinMerged, "duplicatesinMergedfromStreetlink.csv")
