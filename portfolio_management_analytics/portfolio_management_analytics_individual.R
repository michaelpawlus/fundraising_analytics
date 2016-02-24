## load libraries
## if you do not have these installed use: install.packages("ggplot2"), for example, to downoad the library
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(plyr)


#### for this individual report x-axis values either need to remain numeric or use a different plot style


## set working directory
# setwd("C:/Users/pawlusm/Desktop/decTree/fundraising_analytics/portfolio_management_analytics")
setwd("C:/Users/pawlusm/Desktop")

## read in the file
act2 <- read.csv("action_eval3.csv", stringsAsFactors = FALSE)
x <- "Smith"
act2 <- act2[act2$goLast==x,]

#### if you want to use this exact script with no edits then you will need the following colum headers:

#[1] "coreid"        "FullName"      "LastName"      "GradYr"        "const"         "cae"           "parent"       
#[8] "median_income" "RE_Val"        "JobName"       "JobTitle"      "AffTtl"        "Pledge_Blc"    "LubYrs"       
#[15] "FY16"          "FY15"          "FY14"          "Total_Giving"  "low_rating"    "high_rating"   "goLast"       
#[22] "actDesc"       "actDate"       "category"      "mode"          "actText"

#### I think these are mostly self-explanatory (I'll add more deatil in a Markdown file later)

#### If there is high demand, I can actually trim this down.  There are columns that I don't use

#### I will also see about getting together a sample dataset that doesn't have any personal details



#### make cuts to put real estate values into buckets


## check the range of values
range(act2$RE_Val, na.rm = TRUE)

## convert all missing values to zero
act2[is.na(act2)]   <- 0

## put real estate values into $100,000 buckets
act2$re_grp <- cut(act2$RE_Val, breaks = seq(-1, 999999, by = 100000), label=FALSE)

## code the outliers (all zeroes are coded as zeroes and those over $1M are coded as 11)
act2$re_grp[act2$RE_Val==0] <- 0
act2$re_grp[is.na(act2$re_grp)]   <- 11

## convert rating to factor and then reorder factor levels
act2$re_grp <- as.factor(as.character(act2$re_grp))
act2<- within(act2, re_grp <- reorder(re_grp, as.numeric(as.character(re_grp))))

## plot results (faceted bar plot)
ggplot(act2, aes(x=as.numeric(as.character(re_grp)), fill="#FF6666")) + 
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=median(as.numeric(as.character(re_grp)), na.rm=T)),   # Ignore NA values for mean
           color="red", linetype="dashed", size=1) +
  theme_bw() + 
  labs(title="Actions by RE")

#### make cuts to put total giving values into buckets


## check the range of values
range(act2$Total_Giving)  

## put total giving values into $10,000 buckets
act2$tg_grp<- cut(act2$Total_Giving, breaks = seq(0, 100000, by = 10000), label=FALSE)

## code the outliers (all zeroes are coded as zeroes and those over $100,000 are coded as 11)
act2$tg_grp[act2$Total_Giving==0] <- 0
act2$tg_grp[is.na(act2$tg_grp)]   <- 11

## convert rating to factor and then reorder factor levels
act2$tg_grp <- as.factor(as.character(act2$tg_grp))
act2<- within(act2, tg_grp <- reorder(tg_grp, as.numeric(as.character(tg_grp))))

## plot results
ggplot(act2, aes(x=as.numeric(as.character(tg_grp)), fill="#FF6666")) + 
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=median(as.numeric(as.character(tg_grp)), na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  theme_bw() + 
  labs(title="Actions by TG")


#### histogram by affinity without buckets  
## (may add a 2-pass bucketing in a future iteration -- only for those greater than 10)


act2$AffTtl <- as.factor(as.character(act2$AffTtl ))
act2<- within(act2, AffTtl <- reorder(AffTtl, as.numeric(as.character(AffTtl))))

## plot results
ggplot(act2[ which(act2$AffTtl!=0),], aes(x=as.numeric(as.character(AffTtl)), fill="#FF6666")) + 
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=median(as.numeric(as.character(AffTtl)), na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  theme_bw() + 
  labs(title="Actions by Affinity")


#### get the mean word count


## make a column of all zeroes (this is where your word count will go)
act2$wrdc <- rep(0,nrow(act2))

## make another column for concatenated strings and fill it in with all "x"s for now.
## this is needed if you have multiple fields that are used for narrative text in contact reports
## this column will hold the combined text
act2$conc <- rep("x",nrow(act2))

## concatenate description and comment fields which are the two text fields in Millennium that are used
for (i in 1:nrow(act2)) {
  act2[i,30] <- paste(act2[i,22], act2[i,26], sep = " ")  # change columns conc gets two text fields
}

## word count for each
## this goes row by row and splits the text field by spaces (" ") seperating each word
## it then counts the number of individual words
for (i in 1:nrow(act2)) {
  y <- act2[i,30]
  z <- strsplit(y, " ")
  act2[i,29] <- length(z[[1]])  
}

## this creates a data frame of mean values based on the word counts for each gift officer
mm <- ddply(act2, "goLast", summarise, mwrds = mean(wrdc))

mm$mwrds


#### plot giving by category

#### make side by side bars  ####

ggplot(act2, aes(goLast, fill=category)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))


#### plot giving by mode

#### make side by side bars  ####

qplot(factor(goLast), data=act2, geom="bar", fill=factor(mode)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))


#### make a word cloud


## load in additional libraries
library(tm)
library(SnowballC)
library(wordcloud)

#### global word cloud  (for all gift officers collectively)

## set up the image window  (this might only be necessary if reseting after doing the by gift officer view below)
par(mfrow = c(1,1))

##### word clouds are new to me so my comments below are what I think is happening

## create a corpus of word objects from the text column 
actCorpus <- Corpus(VectorSource(act2$conc))

## map the words to a plain text doc
actCorpus <- tm_map(actCorpus, PlainTextDocument)

## this removes punctuation and common words like "the", "it", etc. (I believe)
actCorpus <- tm_map(actCorpus, removePunctuation)
actCorpus <- tm_map(actCorpus, removeWords, stopwords('english'))

## this will look to find root words and match them i.e.: go, goes, going will all get grouped as one word element
actCorpus <- tm_map(actCorpus, stemDocument)

## create word cloud
wordcloud(actCorpus,
          scale = c(3,.1),
          max.words = 25, 
          min.freq = 5,
          random.order = FALSE,
          colors = brewer.pal(9, 'Blues')[4:9]
)

#### sentiment analysis

## load library
library(syuzhet)

#### global emotion chart (for all gift officers collectively)

## put text data in its own vector
ocomm <- act2$conc

## get the emotional data by checking words against emotion-based taxonomy
d<-get_nrc_sentiment(ocomm)

## transpose the data frame (make columns into rows and rows into columns)
td<-data.frame(t(d))

## get a numerican sum for each emotion based on the count from each action
td_new <- data.frame(rowSums(td[2:ncol(td)]))

## rename the first column heading for td_new
names(td_new)[1] <- "count"

## column bind the rownames with the emotion names for td_new as a column called sentiment
td_new <- cbind("sentiment" = rownames(td_new), td_new)

## remove row names since they are duplicated now
rownames(td_new) <- NULL

## td_new2 contains the first 8 values which are the different emotions
td_new2<-td_new[1:8,]

## td_new3 contains the last 2 rows which are the positive or negative sentiment
td_new3<-td_new[9:10,]

#plot emotions
qplot(sentiment, data=td_new2, weight=count, geom="histogram",fill=sentiment)+ggtitle("Emotional Sentiment")
#plot +/- sentiment
qplot(sentiment, data=td_new3, weight=count, geom="histogram",fill=sentiment)+ggtitle("Postive/Negative Sentiment")

## if you notice any unusual trends you can check comments for any person using the snippet below

# act2$conc
