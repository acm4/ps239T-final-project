##New angle on Coding project---diff documents and filters in ProQuest

#import packages for dataframe manipulation and data visualization
library(ggplot2)
library(dplyr)

#import data set as dataframe and merge csv files into one
# file_list <- list.files(path = ("C:/Users/Amani/Documents/kitchenette1_ProQuestDocuments-2016-12-05.csv", 
#                         "C:/Users/Amani/Documents/kitchenette2_ProQuestDocuments-2016-12-05.csv",
#                         "C:/Users/Amani/Documents/kitchenette3_ProQuestDocuments-2016-12-05.csv",
#                         "C:/Users/Amani/Documents/kitchenette4_ProQuestDocuments-2016-12-05.csv",
#                         "C:/Users/Amani/Documents/kitchenette5_ProQuestDocuments-2016-12-05.csv"), 
#                         all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)
# list_of_files <- lapply(file_list, read.csv) # Read in each file
# merge_all(list_of_files, by = "Name")
###Can't get to work; multiple irresolvable errors

##Read csvs into R  
data1 <- read.csv("C:/Users/Amani/Documents/Coding/kitchenette1_ProQuestDocuments-2016-12-04.csv")
data2 <- read.csv("C:/Users/Amani/Documents/Coding/kitchenette2_ProQuestDocuments-2016-12-04 (1).csv")
data3 <- read.csv("C:/Users/Amani/Documents/Coding/kitchenette3_ProQuestDocuments-2016-12-05.csv")
data4 <- read.csv("C:/Users/Amani/Documents/Coding/kitchenette4_ProQuestDocuments-2016-12-05.csv")
data5 <- read.csv("C:/Users/Amani/Documents/Coding/kitchenette5_ProQuestDocuments-2016-12-05.csv")

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
  ## Will convert back to csv to manually remove then re-read csv into df. Annoying. Would need to find better method if larger data set.

write.csv(newuniquedocs_df, "ProquestFiles-toclean_1940-59.csv", row.names = FALSE)
  ## cleaned duplicates; found that some were included because of slight diffs in Abstracts column (some sentences had an extra space or punctuation mark)

## re-read csv file back into R df
finalunique_df <- read.csv("C:/Users/Amani/Documents/ProquestFiles-toclean_1940-59.csv")
  ## Returns 453 of the 465 rows from pre-cleaned df. One duplicate article left in only bc it appears in both the Daily Defender and Chicago Defender (National ed.)--so technically not a duplicate.
  ## This is the polished dataset on which to begin performing analyses.


## import necessary packages
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

##Analyze data from Titles column

#Pre-process data
titles <- removePunctuation(as.character(finalunique_df$uniquedocs_df.Title)) #could not run function as a factor, so changed to character vector
titles <- tolower(titles) #convert to lowercase
titles <- removeWords(titles, stopwords(kind = "en")) #remove English stopwords
titles <- stripWhitespace(titles)

#Turn dataframe into document-term matrix
##titledtm <- DocumentTermMatrix(titles)  #gives error "no applicable method for 'TermDocumentMatrix' applied to an object of class "character"
##titledtm <- termFreq(titles) #will not work because not a TextDocument
##titledtm <- as.DocumentTermMatrix(titles, weighting = weightTfIdf) #doesn't work
  ###maybe don't need a dtm because data already organized when exported from ProQuest

# #organize data by frequency
# ## titles2 <- as.matrix(titles) ##not what I want
# titles2 <- removePunctuation(as.character(finalunique_df))
# titles2 <- tolower(titles2)
# titles2 <- removeWords(titles2, stopwords("english"))
# titles2 <- stripWhitespace(titles2)
  ##realized I could just use the first value and input that into the wordcloud function; was thinking I needed to do a lot of converting and organizing data that was actually unnecessary. Wasted a few hours on this, but figured it out.

#Create wordcloud for overall term frequency
wordcloud(titles, scale=c(5,0.5), max.words=100,
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
          colors = brewer.pal(6, "Set1"))

#Get rid of word rotation for easier read 
wordcloud(titles, scale=c(5,0.5), max.words=100, 
          random.order=FALSE, rot.per=0, use.r.layout=FALSE, 
          colors = brewer.pal(6, "Set1"))

#View more words in cloud by reducing largest size in scale
wordcloud(titles, scale=c(4,0.5), max.words=100,
          random.order=FALSE, rot.per=0, use.r.layout=FALSE, 
          colors = brewer.pal(6, "Set1"))

#Get clearer sense of frequent words by increasing minimum frequency
wordcloud(titles, scale=c(4,0.5), max.words=100, min.freq = 5,
          random.order=FALSE, rot.per=0, use.r.layout=FALSE, 
          colors = brewer.pal(6, "Set1"))
 
########

#Analyze Abstracts column


str(finalunique_df$uniquedocs_df.Abstract) ##291 levels
summary(finalunique_df$uniquedocs_df.Abstract)
## !is.na(finalunique_df$uniquedocs_df.Abstract) ##yields all TRUE, which is incorrect-- wrong function to count missing values?
## sum(!is.na(finalunique_df$uniquedocs_df.Abstract)) ##yields 453, which is incorrect, as some rows are blank in this column

#Pre-process data
abstracts <- removePunctuation(as.character(finalunique_df$uniquedocs_df.Abstract), 
                               preserve_intra_word_dashes = TRUE) 
abstracts <- tolower(abstracts) 
abstracts <- removeWords(abstracts, stopwords(kind = "en")) 
abstracts <- removeWords(abstracts, c("will", "can", "get", "like", "take", "may")) #removed after making initial wordcloud because irrelevant
abstracts <- stripWhitespace(abstracts)
abstracts

#Stemmed data
# abstractsstem <- stemDocument(abstracts, language = "porter")
# abstractsstem
  ## Only stems last word in each abstract rather than all words; tried other functions but got error msgs

#Create wordcloud
wordcloud(abstracts, scale=c(4,0.5), max.words=100,
          random.order=FALSE, rot.per=0, use.r.layout=FALSE, 
          colors = brewer.pal(8, "Set1"))

#Create wordcloud removing "kitchenette" and "apartment" and others to visualize less-expected associated words
abstracts2 <- removeWords(abstracts, c("kitchenette", "kitchenettes", "apartment", 
                                       "apartments", "week", "story", "last",
                                       "said", "say", "just", "ave"))
abstracts2 <- stripWhitespace(abstracts2)
wordcloud(abstracts2, scale=c(3.8,0.5), max.words=100, min.freq = 4,
          random.order=FALSE, rot.per=0, use.r.layout=T, 
          colors = brewer.pal(8, "Set1"))  ##use.r.layout=T seems to visualize more words
  ##Not particularly informative


#######

# Analyze national data (from all selected newspapers)

## Create data frame with article counts by city
freq_df <- data.frame(table(finalunique_df$uniquedocs_df.placeOfPublication)) 

## Visualize data in barplot because categorical
par(mfrow=c(1.2, 1), mar=c(9, 4.5, 3, 4)) ##plot parameters
barplot(freq_df$Freq, ylab = "Total Number", 
        main = "Kitchenette Newspaper Articles by City, 1940-59", 
        names.arg = freq_df$Var1, las = 2, col = "skyblue") 
  ## las = 2 will render bar labels perpendicular to x-axis
  ## No x-axis label; xlab overlaps with bar labels, even with some manipulation
# This visualization could demonstrate why Chicago is a key city for kitchenette study; Chicago has 250(+?) articles


#Create data frame with article counts by publication (to separate Tribune from Defender)
freqbypub_df <- data.frame(table(finalunique_df$uniquedocs_df.pubtitle), stringsAsFactors = FALSE)  
freqbypub_df

#Simplify name values for visualization (omit years of pub)
# freqbypub_df <-  removeNumbers(freqbypub_df$Var1)
# freqbypub_df <- removePunctuation(freqbypub_df)
# freqbypub_df <- removeWords(freqbypub_df, words = c("National Edition", "Daily edition", "Current File"))
  ## Object errors

#Try another simplification
# freqbypub_df$Var1[1] <- "Chicago Daily Tribune"
# freqbypub_df$Var1[2] <- "Daily Defender"
# freqbypub_df$Var1[3] <- "Los Angeles Times"
# freqbypub_df$Var1[4] <- "New York Times"
# freqbypub_df$Var1[5] <- "Chicago Defender"
# freqbypub_df$Var1[6] <- "Washington Post"
# freqbypub_df$Var1[7] <- "Washington Post & Times Herald"
  ## Error msg and generates NAs in place of values

##Convert Var1 from factor to character vector; http://combine-australia.github.io/r-novice-gapminder/05-data-structures-part2.html
freqbypub_df$Var1 <-  as.character(freqbypub_df$Var1)

## Simplify names (3rd attempt)
freqbypub_df$Var1[1] <- "Chicago Daily Tribune"
freqbypub_df$Var1[2] <- "Daily Defender"
freqbypub_df$Var1[3] <- "Los Angeles Times"
freqbypub_df$Var1[4] <- "New York Times"
freqbypub_df$Var1[5] <- "Chicago Defender"
freqbypub_df$Var1[6] <- "Washington Post"
freqbypub_df$Var1[7] <- "Wash. Post & Times Herald"
  ##success!
  
#Visualize data
par(mfrow=c(1, 1), mar=c(4.5, 11, 4, 7))  
barplot(freqbypub_df$Freq, names.arg = freqbypub_df$Var1, 
        main = "Kitchenette Articles by Publication, 1940-59", 
        xlab = "Total Number", las = 1, horiz = TRUE,
        col = "purple", space= 0)
  ##horizontal chart makes publication titles easier to read


####
#Analyze change over time

##Subset data by article frequency per year
freqyr_df <- data.frame(table(finalunique_df$uniquedocs_df.year))
freqyr_df

##Visualize data with line plot
timeplot <- ggplot(freqyr_df, aes(Var1, Freq, show.legend = FALSE, group = 1)) + 
  geom_point(colour = "blue") +  geom_line(colour = "blue", size = 3) + 
  +   ggtitle("Kitchenette Newspaper Articles by Year, 1940-59") +
  +   labs(x="Year",y="Number of Articles") + 
  theme(plot.title = element_text(color="#666666", 
                                  face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(family = "sans serif", color="#666666", face="bold", size=16)) 
    ##how to title plot: modified from https://www.r-bloggers.com/how-to-format-your-chart-and-axis-titles-in-ggplot2/
    ##how to remedy "group aesthetic" error by using group=1: http://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation

timeplot

## Visualize proportions of yearly publication
# stackedtimeplot <- ggplot(freqyr2_df, aes(Var2, Freq, fill = Var1, position = "stack")) + geom_col() + 
#   ggtitle("Kitchenette Articles Publication by Proportion, 1940-59") +
#   labs(x="Year",y="Total Number") + theme(legend.title = element_blank()) +
#   theme(plot.title = element_text(family = "sans serif", color="#666666", 
#                                   face="bold", size=20, hjust=0)) + 
#   theme(axis.title = element_text(family = "sans serif", color="#666666", face="bold", size=16))
  ##Not useful visualization because does not accurately represent the number of articles for each publication

## Plot with lines to visualize contrasting yearly outputs of publications
lines <- ggplot(freqyr2_df, aes(Var2, Freq, group = Var1, colour = Var1)) + geom_line(size = 2) + 
  ggtitle("Yearly Output of Kitchenette Articles by Newspaper, 1940-59") +
  labs(x="Year",y="Total Number") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(color="#666666", face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(color="#666666", face="bold", size=16))
lines

##Include points and lines to show discrete amounts along line
# lines2 <- ggplot(freqyr2_df, aes(Var2, Freq, group = Var1, colour = Var1)) + geom_line(size = 2)  +
#   geom_point(aes(shape = factor(Var1))) + geom_point(aes(colour = factor(Var1), size = 2)) +
#   ggtitle("Yearly Output of Kitchenette Articles by Newspaper, 1940-59") +
#   labs(x="Year",y="Total Number") + theme(legend.title = element_blank()) +
#   theme(plot.title = element_text(color="#666666", face="bold", size=20, hjust=0)) +
#   theme(axis.title = element_text(color="#666666", face="bold", size=16))
# lines2
    ## Legend is messy and doesn't use shapes when plotting, not sure why

##Include points and lines to show discrete amounts along line (remove shapes)
lines2 <- ggplot(freqyr2_df, aes(Var2, Freq, group = Var1, colour = Var1)) + geom_line(size = 2) + 
  geom_point(aes(colour = factor(Var1), size = 2)) +
  ggtitle("Yearly Output of Kitchenette Articles by Newspaper, 1940-59") +
  labs(x="Year",y="Number of Articles") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(color="#666666", face="bold", size=18, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=16))
lines2


#######
##Sentiment Analysis

library(qdap)
# library(tm) ##already loaded

#Create simplified dataframe with 4 variables (title, yr, city, pub)
titlepubdf <-  data.frame(finalunique_df$uniquedocs_df.Title, finalunique_df$uniquedocs_df.pubtitle, 
                          finalunique_df$uniquedocs_df.placeOfPublication, finalunique_df$uniquedocs_df.year,
                          stringsAsFactors = FALSE)
  
#Create dataframe with only Chicago newspapers
# chidata_df <- titlepubdf[titlepubdf$finalunique_df.uniquedocs_df.placeOfPublication=="Chicago, Ill.", ]
# (chidata_df <- titlepubdf[titlepubdf$finalunique_df.uniquedocs_df.placeOfPublication 
#                             %in% "Chicago, Ill.", ])
# (chidata_df <- titlepubdf[with(titlepubdf, finalunique_df.uniquedocs_df.placeOfPublication == "Chicago, Ill."), ])
# chidata_df <- subset(titlepubdf, finalunique_df.uniquedocs_df.placeOfPublication == "Chicago, Ill.")
# chidata_df <- titlepubdf[as.character(titlepubdf$finalunique_df.uniquedocs_df.placeOfPublication=="Chicago, Ill."), ] ##returns 4 columns and all rows but all populated with NAs
   ##Spent hours on this basic subsetting. None of the codes works properly. Tried various manipulations of code, as well as subsetting by different column data.
   ##Returns dataframe with 0 columns and all rows or 4 columns and 0 rows, depending on where comma is placed. None yield populated df.

##Create new df with data ordered by publication title in order to subset by index (using dplyr arrange function)
ordered_df <- arrange(titlepubdf, finalunique_df.uniquedocs_df.pubtitle, 
                      finalunique_df.uniquedocs_df.year) 
              #http://stackoverflow.com/questions/14817620/how-to-sort-a-data-frame-by-alphabetic-order-of-a-character-variable-in-r

#Create new dfs for Chicago Tribune and Chicago Defender(national and daily editions concatenated to balance numbers with Tribune)
tribune_df <- ordered_df[1:155,]
defender_df <- ordered_df[c(156:175, 330:404),]

#Merge back into one df
chidata_df <- rbind(tribune_df, defender_df) #yields 250 observations

####Visualize data

#Pre-process data
chititles <- removePunctuation(as.character(chidata_df$finalunique_df.uniquedocs_df.Title))
chititles <- tolower(chititles) 
chititles <- removeWords(chititles, stopwords(kind = "en")) 
chititles <- stripWhitespace(chititles)
  #same as above

#Create wordcloud for inital visualization (for comparison with national results)
wordcloud(chititles, scale=c(3.8,0.5), max.words=100, 
          random.order=FALSE, rot.per=0, use.r.layout=T, 
          colors = brewer.pal(8, "Set1"))
  #Many similarities in highest frequency words; however, "fire", "blaze", and "simple" are more prominent, 
  #as are words relating to death (killed, dies, die, death, shot)


######
#Perform Sentiment Analysis

#Calculate polarity
# tribunepol <- with(tribune_df, polarity(finalunique_df.uniquedocs_df.Title, finalunique_df.uniquedocs_df.year))
  ##Tried to replicate from Rochelle's code; error--- no polarity function found

#Download sentimentr package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr)
  #https://github.com/trinker/sentimentr#installation

#Calculate Tribune sentiment
tribsent <- sentiment(text.var = tribune_df$finalunique_df.uniquedocs_df.Title, 
                      polarity_dt = polarity_table) 
    ##Yields 4col df organized by element ID, sentence ID, word count, and sentiment score (polarity)
    ##Most titles had sentiment score of 0
tribsent

#Plot aggregate Tribune sentiment
plot(tribsent)
    ##has built in xlab = "Duration" (by percentage) and ylab = "Emotional Valence" (from -20 to 35)
    ##Not sure how to decipher plot, and documentation isn't clear; not particularly useful for me because can't understand
    ##Duration = across the total number of years (1940-1959)??

#Plot using other barplots
barplot(tribsent$sentiment)
bar_tribsent <- ggplot(tribsent, aes(element_id, sentiment, show.legend = FALSE, fill = "")) + geom_col() + 
                         ggtitle("Chicago Tribune Sentiment Scores") +
                         labs(x="Article ID (Num > as Year >)",y="Sentiment/Polarity Score") + 
                         theme(plot.title = element_text(color="#666666", 
                                                         face="bold", size=20, hjust=0)) + 
                         theme(axis.title = element_text(color="#666666", 
                                                         face="bold", size=16))
bar_tribsent

#Highlight sampling of polarized words
  # highlight(with(subset(tribune_df, row.names %in% 
  #                         sample(unique(row.names), 3)), 
  #                sentiment_by(finalunique_df.uniquedocs_df.Title, row.names)))
  ##Gives error: "unique() applies only to vectors"

  #Create new df with row names included
  tribune_df2 <- data.frame(tribune_df, row.names = T)
    #Error: duplicate.names

    #Fix by making unique names with rownames(df) = make.names(nams, unique=TRUE)  #https://www.biostars.org/p/62988/
    # tribune_df2 <- data.frame((rownames(tribune_df) = 
    #                          make.names(tribune_df$finalunique_df.uniquedocs_df.Title 
    #                                     == ("White Collar GIRL" 
    #                                         | "VOICE OF THE PEOPLE"), unique=TRUE))
    #                       (tribune_df, row.names = T))
        ##Error: "operations are possible only for numeric, logical or complex types"

  #Fix by getting rid of duplicate row names (only 2) because both have neutral polarity and simpler than finding new function
    # tribune_df2 <- data.frame(tribune_df[tribune_df$finalunique_df.uniquedocs_df.Title, 
    #                                      make.unique(row.names = T)])  ##error
  tribune_df2 <- tribune_df[!duplicated(tribune_df$finalunique_df.uniquedocs_df.Title),]
    ##http://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
  
  #Add variable for row number to use highlight function
  tribune_df2$rownumber <- 1:nrow(tribune_df2)
    #http://stackoverflow.com/questions/17732728/referencing-row-number-in-r
  
  
#Try highlighting again by polarity of random sample of Tribune articles
  highlight(with(subset(tribune_df2, rownumber %in% sample(unique(rownumber), 3)), 
                 sentiment_by(finalunique_df.uniquedocs_df.Title, rownumber)))
    ##Works, but my sample yields results with neutral polarity. Should be a way to subset by sentiment/polarity score, 
    ##but can't figure out how to append column with score because of diff row lengths.
    ##For sake of this dataset, will manually select data with pos or neg polarity from sentiment df.

# Highlight by polarity of specified sample (looked in tribsent2 df for polarity scores)
  #Negative polarity
  highlight(with(subset(tribune_df2, rownumber == c(73, 14)), 
                 sentiment_by(finalunique_df.uniquedocs_df.Title, rownumber)))
    ##Opens visualization in a browser window. Works but a bit buggy; won't return more than two 
    ##results, even if multiple rownumbers, and won't return pos and neg in same call
  
  #Positive polarity
  highlight(with(subset(tribune_df2, rownumber == c(115, 24)), 
                 sentiment_by(finalunique_df.uniquedocs_df.Title, rownumber)))

######
#Analyze Defender Sentiment
  
#Calculate Defender sentiment
  defendersent <- sentiment(text.var = defender_df$finalunique_df.uniquedocs_df.Title, 
                        polarity_dt = polarity_table)
  defendersent

#Visualize Defender sentiment
  bar_defendersent <- ggplot(defendersent, aes(element_id, sentiment, show.legend = FALSE, fill = "")) + geom_col() + 
    ggtitle("Chicago Defender Sentiment Scores") +
    labs(x="Article ID (Num > as Year >)",y="Sentiment/Polarity Score") + 
    theme(plot.title = element_text(color="#666666", 
                                    face="bold", size=20, hjust=0)) + 
    theme(axis.title = element_text(color="#666666", 
                                    face="bold", size=16))
  bar_defendersent
  
##Pre-process data to highlight polarities 
  
#Create new df with row names included
  defender_df2 <- data.frame(defender_df, row.names = T)
    #Error: duplicates

  #Get rid of duplicates
  defender_df2 <- defender_df[!duplicated(defender_df$finalunique_df.uniquedocs_df.Title),]
  defender_df2 ##yields 87 of the 95 from original df; only 1 polarized observation removed
  
  #Add variable for row number to use highlight function
  defender_df2$rownumber <- 1:nrow(defender_df2)
  
#Highlight Defender sentiments
  #random sample
  highlight(with(subset(defender_df2, rownumber %in% sample(unique(rownumber), 5)), 
                 sentiment_by(finalunique_df.uniquedocs_df.Title, rownumber)))
      ##worked better than with Tribune--returned both pos and neg in same call; 
      ##may be due to increased sample size and nature of data set




  