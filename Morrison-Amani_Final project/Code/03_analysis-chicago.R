## Analysis for Chicago Newspaper Data ##

##This script walks through analyses of the data from Chicago newspapers (Tribune; Defender and Daily Defender).

#######
##Sentiment Analysis

library(qdap)
# library(tm) ##already loaded


#Create simplified dataframe with 4 variables (title, yr, city, pub)
titlepubdf <-  data.frame(finalunique_df$uniquedocs_df.Title, finalunique_df$uniquedocs_df.pubtitle, 
                          finalunique_df$uniquedocs_df.placeOfPublication, finalunique_df$uniquedocs_df.year,
                          stringsAsFactors = FALSE)

## Create dataframe with only Chicago newspapers
# chidata_df <- titlepubdf[titlepubdf$finalunique_df.uniquedocs_df.placeOfPublication=="Chicago, Ill.", ]
# (chidata_df <- titlepubdf[titlepubdf$finalunique_df.uniquedocs_df.placeOfPublication 
#                             %in% "Chicago, Ill.", ])
# (chidata_df <- titlepubdf[with(titlepubdf, finalunique_df.uniquedocs_df.placeOfPublication == "Chicago, Ill."), ])
# chidata_df <- subset(titlepubdf, finalunique_df.uniquedocs_df.placeOfPublication == "Chicago, Ill.")
# chidata_df <- titlepubdf[as.character(titlepubdf$finalunique_df.uniquedocs_df.placeOfPublication=="Chicago, Ill."), ] ##returns 4 columns and all rows but all populated with NAs
    ##Spent hours on this. None of the codes works properly. Tried various manipulations of code, as well as subsetting by different column data.
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
  ##Has built in xlab = "Duration" (by percentage) and ylab = "Emotional Valence" (from -20 to 35)
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
defender_df2   ##yields 87 of the 95 from original df; only 1 polarized observation removed

#Add variable for row number to use highlight function
defender_df2$rownumber <- 1:nrow(defender_df2)

#Highlight Defender sentiments
  
#random sample
  highlight(with(subset(defender_df2, rownumber %in% sample(unique(rownumber), 5)), 
               sentiment_by(finalunique_df.uniquedocs_df.Title, rownumber)))
    ##Worked better than with Tribune--returned both pos and neg in same call; 
    ##may be due to increased sample size and nature of data set




