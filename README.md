# StreetlinkDatakind2017
Cleaning data for Streetlink Datadive 2017

For the Data Ambassador team to get access to files made during data cleaning. 

* 01_StreetLinkMergeDatac.R is based on earlier work by Linda, it takes the data files and creates a merged file across Merged (Gareth's "clean" data), All Streetlink, and All CHAIN. We consider Merged to be the "ground truth" based on his cleaning, "All CHAIN" is next in quality because there is some work done on unifying duplicates etc, and "All Streetlink" has some unique information but if we need to make a choice, my research suggests it usually won't win. 
* StreetLinkCleaningMerged.R starts cleaning up that file by uniting columns from different sources, cleaning names of outcomes, flagging freetext columns, etc. IN PROGRESS at 20170716
* UserDetailsCleaning.R is cleaning on the User details to identify repeat users. 

Still very much in progress, but the first step of creating the full data file from three sources, i'm reasonably confident is better than earlier versions and a good base "everything" file for later cleaning! 
