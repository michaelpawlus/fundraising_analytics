## load library(ies)
library(stringr)

## set directory
setwd("C:/Users/pawlusm/Desktop")

## read file
gfs <- read.csv("2015 December GradFest.csv", stringsAsFactors = FALSE)

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
write.csv(top.profs, "top_profs.csv")
