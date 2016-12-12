## Load and Clean Data ##

# This script walks through loading and cleaning data into an analysis dataset. 

# Data was collected on ProQuest Historical Newspapers database (not scrapeable) using targeted 
# search term "kitchenette" and filters for year range and document type.
# Then exported as xls files and converted to csvs. Each file contains 100 observatations.

#Import package
library(dplyr)


#Import data set as dataframe and merge csv files into one
# file_list <- list.files(path = ("C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette1_proquest.csv", 
#                         "C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette2_proquest.csv",
#                         "C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette3_proquest.csv",
#                         "C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette4_proquest.csv",
#                         "C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette5_proquest.csv"), 
#                         all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
# list_of_files <- lapply(file_list, read.csv) # Read in each file
# merge_all(list_of_files, by = "Name")
###Can't get to work; multiple irresolvable errors

##Read csvs into R separately, then merge
data1 <- read.csv("C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette1_proquest.csv")
data2 <- read.csv("C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette2_proquest.csv")
data3 <- read.csv("C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette3_proquest.csv")
data4 <- read.csv("C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette4_proquest.csv")
data5 <- read.csv("C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/kitchenette5_proquest.csv")

## rbind to combine all into one
kitchenette_df <- rbind(data1, data2, data3, data4, data5, stringsAsFactors = FALSE) #make sure strings are recognized as character vectors

## Remove any duplicate articles
uniquedocs_df <- unique(kitchenette_df)
uniquedocs_df
summary(uniquedocs_df)
str(uniquedocs_df)
## Yields 486 of orig. 500 results. Not foolproof because when viewed, the df has some duplicate articles that have 
## different ID #s and page numbers. Perhaps will try new df without these variables (since irrelevant for analysis), 
## then use unique method.

## Subset by relevant variables to rid df of extraneous data
uniquedocs2_df <- data.frame(uniquedocs_df$Title, uniquedocs_df$Abstract, uniquedocs_df$Authors,
                             uniquedocs_df$placeOfPublication, uniquedocs_df$pubdate,
                             uniquedocs_df$pubtitle, uniquedocs_df$year)

## Try unique method again to rid of duplicate articles
newuniquedocs_df <- unique(uniquedocs2_df)
newuniquedocs_df
## Returns 465 of 486 rows. Upon viewing, still did not remove some duplicates, but can't figure out why. 
## Will convert back to csv to manually remove then re-read csv into df. Would need to find better method if larger data set.

write.csv(newuniquedocs_df, "ProquestFiles-toclean_1940-59.csv", row.names = FALSE)
## cleaned duplicates; found that some were included because of slight differences in Abstracts column (some sentences had an extra space or punctuation mark)

## re-read csv file back into R df
finalunique_df <- read.csv("C:/Users/Amani/Desktop/ps239T-final-project/Morrison-Amani_Final project/Data/analysis-dataset-kitchenette_proquest.csv")
## Returns 453 of the 465 rows from pre-cleaned df. One duplicate article left in only bc it appears in both the Daily Defender and Chicago Defender (National ed.)--so technically not a duplicate.
## This is the polished dataset on which to begin performing analysis.