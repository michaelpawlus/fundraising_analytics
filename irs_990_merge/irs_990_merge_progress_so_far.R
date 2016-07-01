library(RJSONIO)
library(xml2)

# library(jsonlite)
# 
# (this would pull in the json data put I am runing into memory issues trying to get everything)
# json_file <- "https://s3.amazonaws.com/irs-form-990/index.json"
# json_data <- fromJSON(paste(readLines(json_file), collapse=""))
# 
# json_data <- fromJSON(file=json_file)

## here are a few sample rows
json <- 
  '[
{"EIN": "742661023", "SubmittedOn": "2016-02-09", "TaxPeriod": "201412", "DLN": "93491315003445", "LastUpdated": "2016-06-14T01:20:25.3920704Z", "FormType": "990PF", "ObjectId": "201543159349100344", "OrganizationName": "HARRIET AND HARMON KELLEY FOUNDATION FOR THE ARTS", "IsElectronic": true, "IsAvailable": false},
{"EIN": "562629114", "SubmittedOn": "2016-02-09", "TaxPeriod": "201412", "DLN": "93492310002195", "LastUpdated": "2016-04-29T13:40:20", "URL": "https://s3.amazonaws.com/irs-form-990/201543109349200219_public.xml", "FormType": "990EZ", "ObjectId": "201543109349200219", "OrganizationName": "BROWN COMMUNITY DEVELOPMENT CORPORATION", "IsElectronic": true, "IsAvailable": true},
{"EIN": "270678774", "SubmittedOn": "2016-02-09", "TaxPeriod": "201509", "DLN": "93492308002265", "LastUpdated": "2016-03-21T17:23:53", "URL": "https://s3.amazonaws.com/irs-form-990/201513089349200226_public.xml", "FormType": "990EZ", "ObjectId": "201513089349200226", "OrganizationName": "KIWANIS CLUB OF GLENDORA PROJECTS FUND INC", "IsElectronic": true, "IsAvailable": true}
]'

## convert to a data frame
mydf <- fromJSON(json)

## an empty vector to hold all the URLs
flist <- character()

## loop through and collect all the URLs from that flist vector
for (i in 1:3) {
  flist[i] <- mydf[[i]]$URL
}

## remove any NAs where there is no digital file (i.e. no URL is available)
flist <- flist[!is.na(flist)]

## the next part is just me trying to hack something together and eventually build into the loop
## there are xml packages for parsing this type of file but I can't get them to work

## this is what I have been able to do which is not much

## read the xml file from the first URL which loads in R as a list
x <- read_xml(flist[1])

## get the header text
htxt <- xml_text(xml_children(x)[[1]], trim = TRUE)

## and the text from the body the document
btxt <- xml_text(xml_children(x)[[2]], trim = TRUE)

## create a list of all discrete data elements from the top of the form
hvec <- strsplit(as.character(htxt), "\n")

## create a list of all discrete data elements from the body of the document
bvec <- strsplit(as.character(btxt), "\n")

## in this case, I can select the organization name
## however, i feel that the organization name is not always in the same place
## in thery you can select based on teh xml tag but I can't get that to work
oname <- as.character(trimws(hvec[[1]][19]))

## here is the address but again based on where it is on this particular form
## this is not something that would work in a loop
addr <- as.character(trimws(hvec[[1]][24]))

## the end of year assets
eoy <- as.integer(bvec[[1]][25])

##-----------------------------------

## this is where I tried to loop through and populate a data frame with those values above

## it doesn't work but if we could select on xml tag then this would be possible

## however, there would still have to be some way to read in a portion of the json at a time to avoid running out of memory

##-----------------------------------

## make a blank data frame

df <- data.frame(name=character(),
                 addln=character(), 
                 assets=integer(), 
                 stringsAsFactors=FALSE) 

## loop over URLs

## values are not in a uniform location so this doesn't work

for (i in 1:length(flist)) {
  x <- read_xml(flist[i])
  htxt <- xml_text(xml_children(x)[[1]], trim = TRUE)
  btxt <- xml_text(xml_children(x)[[2]], trim = TRUE)
  hvec <- strsplit(as.character(htxt), "\n")
  bvec <- strsplit(as.character(btxt), "\n")
  df[i,1] <- as.character(trimws(hvec[[1]][19]))
  df[i,2] <- as.character(trimws(hvec[[1]][24]))
  df[i,3] <- as.integer(bvec[[1]][25])  
}
