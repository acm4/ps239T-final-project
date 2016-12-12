## Analysis for National Newspaper Data ##

##This script walks through analyses of the entire dataset (results from newspapers in 4 major US cities). 

## Import necessary packages
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
#library(dplyr) ##already loaded


##Analyze Titles

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

#Analyze Article Abstracts


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

# Analyze Article Frequency

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
