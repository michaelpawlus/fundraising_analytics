## load library(ies)
library(stringr)
library(readr)
library(RCurl)  ## this is only needed if pulling in the sample data via URL

## set directory
setwd("C:/Users/pawlusm/Desktop")

#  ## read file  (if you are using your own data -- just change the file name)
#  ## if you want to use this script with no edits then give your free text header the column name: fs_comm
#  gfs <- read.csv("2015 December GradFest.csv", stringsAsFactors = FALSE)
gfs <- read_csv("2016_April_GradFest.csv")
names(gfs) <- c("id","fs_comm")

## read in the sample data
x <- getURL("https://raw.githubusercontent.com/michaelpawlus/fundraising_analytics/master/name_extraction_from_free_text/sample_survey.csv")
gfs <- read.csv(text = x, stringsAsFactors = FALSE)

## regex to extract two adjacent words that both start with a capital letter
names <- "((\\b[A-Z]\\w{2,100}\\s)(\\b[A-Z]\\w{2,100}\\b))"

## create a vector of strings with no special characters from the free text comments
## (to help with name extraction)
words <- str_replace_all(gfs$fs_comm, "[[:punct:]]", "")

## list of all two word instances as mentioned above
two.words <- str_extract_all(words, names)

## list to vector
two.words <- unlist(two.words)

## remove NAs
two.words <- two.words[!(is.na(two.words))] 

## create data frame out of frequency table of unique words
two.tbl <- as.data.frame(table(two.words))  

## subset by some frequency cutoff
top.profs <- two.tbl[which(two.tbl$Freq>3),]

## sort order descending
top.profs <- top.profs[order(-top.profs$Freq),] 

## write file to a csv
write_csv(top.profs, "top_profs.csv")
