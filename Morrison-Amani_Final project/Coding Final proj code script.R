###Code for final project

#import packages for dataframe manipulation and data visualization
library(ggplot2)
library(dplyr)

#import dataset as a dataframe
proqfiles <- read.csv("C:/Users/Amani/Documents/ProQuestDocuments-2016-11-15.csv")

#familiarize with dataset
summary(proqfiles)
str(proqfiles)

#create new dataframe only containing variables that may be relevant for analysis
proqfiles2 <- data.frame(proqfiles$Title, proqfiles$Abstract, proqfiles$Authors, 
                         proqfiles$placeOfPublication, proqfiles$pubdate, 
                         proqfiles$pubtitle, proqfiles$year)

proqfiles2 ##view new dataframe


freq_df <- data.frame(table(proqfiles$placeOfPublication)) ##creates new dataframe with article counts by city

par(mfrow=c(1.2, 1), mar=c(9, 4.5, 3, 4)) ##plot parameters
barplot(freq_df$Freq, ylab = "Total Number", 
        main = "Kitchenette Newspaper Articles by City, 1930-59", 
        names.arg = freq_df$Var1, las = 2) 
##creates bar plot showing article count by city, 
##with y axis label, bar names (perpendicular to x axis), and main chart names included --
##no x axis label because cannot figure out how to get xlab to display below, rather than overlapping with, bar names


##create new data frame with frequency of articles displayed by publication title rather than city, 
##in order to have separate data for Chicago Tribune and Chicago Defender
freqbytitle_df <- data.frame(table(proqfiles$pubtitle), stringsAsFactors = FALSE)  
freqbytitle_df

fivepubs_df <- subset.data.frame(freqbytitle_df, freqbytitle_df$Freq>=5)
##subset data such that dataframe only contains publications with 5 or more kitchenette articles, 
##saving to new dataframe; modified from http://stackoverflow.com/questions/26133532/how-do-i-exclude-rows-in-r-based-on-multiple-values

fivepubs_df


##shorten value that shows for publication titles for cleaner display on data visualization; couldn't figure out if there is an easier way to do this
fivepubs_df$Var1[1] <- "Chicago Daily Tribune" ##for whatever reason gives error msg and generates NA (didn't do this on the first run of the code, but did on 2nd run through)

##convert Var1 from factor to character vector; http://combine-australia.github.io/r-novice-gapminder/05-data-structures-part2.html
fivepubs_df$Var1 <- as.character(fivepubs_df$Var1)

##try again to shorten titles
fivepubs_df$Var1[1] <- "Chicago Daily Tribune"
fivepubs_df$Var1[2] <- "Los Angeles Times"
fivepubs_df$Var1[3] <- "New York Times"
fivepubs_df$Var1[4] <- "Chicago Defender"
fivepubs_df$Var1[5] <- "Washington Post"

fivepubs_df ##check to make sure it worked (it did.)

##visualize data
par(mfrow=c(1, 1), mar=c(4.5, 10, 4, 7))  
barplot(fivepubs_df$Freq, names.arg = fivepubs_df$Var1, 
        main = "Kitchenette Articles by Publication, 1930-59", 
        xlab = "Total Number", las = 1, horiz = TRUE,
        col = "purple", space= 0)
##bar chart shows the total number of kitchenette articles in each of the 5 publications
##horizontal chart makes publication titles easier to read
##no y label because cannot figure out how to prevent bar labels from overlapping it; would be "Publication Title"


##subset data by frequency per year
freqyr_df <- data.frame(table(proqfiles2$proqfiles.year))
freqyr_df

## visualize change over time (univariate); includes freq from all newspapers b/c not sure how to exclude certain ones (spent a lot of time trying to subset)
## Use geom_col instead of geom_bar b/c geom_bar does not plot properly; help function explains why
timeplot3 <- ggplot(freqyr_df, aes(Var1, Freq, show.legend = FALSE, fill = 2)) + geom_col() + 
  ggtitle("Kitchenette Newspaper Articles by Year, 1930-59") +
  labs(x="Year",y="Total Number") + 
  theme(plot.title = element_text(family = "sans serif", color="#666666", 
                                  face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(family = "sans serif", color="#666666", face="bold", size=16)) ##found how to title plot; modified from https://www.r-bloggers.com/how-to-format-your-chart-and-axis-titles-in-ggplot2/


## visualize change over time (bivariate); use stacked bar chart
timeplotstack <- ggplot(freqyr_df2, aes(Var2, Freq, fill = Var1, position = "stack")) + geom_col() + 
  ggtitle("Kitchenette Newspaper Articles by Year, 1930-59") +
  labs(x="Year",y="Total Number") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(family = "sans serif", color="#666666", 
                                  face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(family = "sans serif", color="#666666", face="bold", size=16))

# Need to find a way to subset for frequency of articles and include year info----may need another csv to do that
# Want to visualize frequency of articles by year
freq_df2 <- data.frame(table(proqfiles2$proqfiles.pubtitle, proqfiles2$proqfiles.year), stringsAsFactors = FALSE)
freq_df2

##plot data with bar chart (b/c categorical), using legend to distinguish by publication
timeplot <- ggplot(freq_df2, aes(Var2, Freq, color = Var1)) + geom_point()
timeplot





