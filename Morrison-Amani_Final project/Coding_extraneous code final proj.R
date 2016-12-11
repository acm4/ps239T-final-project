#Coding proj extra unused code

##get rid of occurrences = 0 to have cleaner data
freqyr_df <- subset.data.frame(freq_df2, freq_df2$Freq>0)
freqyr_df2 <- subset.data.frame(freqyr_df, 
                                freq_df2$Var1!=c("Weekly Irish Times (1921-1941)", 
                                                 "Afro-American (1893-1988)", 
                                                 "New York Amsterdam News (1938-1941)", 
                                                 "The Irish Times (1923-Current File)", 
                                                 "The Manchester Guardian (1901-1959)", 
                                                 "The Washington Post and Times Herald (1954-1959)")) ##error messageabout nonmatching object lengths, so can't use

#import packages for text mining and word cloud generation
library(tm)
library(wordcloud2)

##render articles titles in document-term-matrix form
DocumentTermMatrix(proqfiles2$proqfiles.Title) #get error message b/c object class is factor
##must first change titles from factors to character vectors (realize now I should have altered original data frame such that stringsasfactors = FALSE)
as.character(proqfiles2$proqfiles.Title)   

##getting confused on why i need dtm to begin with??? need to have a matrix with word frequency 
##but not sure how to separate words in titles by keyword kitchenette and find frequency 
##of that word and others near it
ktitles_dtm <- DocumentTermMatrix(proqfiles2$proqfiles.Title) #save as document-term-matrix


write.csv(proqfiles2, "ProQuestKitchenette-1930-1959-files.csv", row.names = FALSE)
##convert data frame with relevant variables back to csv

newproqfiles <- read.csv("C:/Users/Amani/Documents/ProQuestDocuments-2016-11-15.csv", 
                         stringsAsFactors = FALSE, header = TRUE)


# ***Could be cool to make contrasting word clouds like this from the DSL at UR: 
#http://dsl.richmond.edu/textmapping/stems/clouds/contrast/Northern%20Republican/Southern%20Former%20Secessionist/negro