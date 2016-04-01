setwd("C:/Users/pawlusm/Desktop")

library(readr)
library(data.table)
library(dplyr)
library(reshape2)
library(bit64)
library(RCurl)

## read in the data
pg <- fread("pg_prospects.csv")

## or read in the sample data from GitHub
x <- getURL("https://raw.githubusercontent.com/michaelpawlus/fundraising_analytics/master/correcting_for_class_imbalance/pg_prospects.csv")
pg <- fread(x)

## convert characters to integers
#pg <- pg[, cae:=as.integer(as.factor(cae))]
#pg <- pg[, const:=as.integer(as.factor(const))]

## convert all zeroes in gillett column to ones
pg <- pg[pg_age > 0, gillett:= 1]

## create an age column that combines pg_age if gillett and age if not (remove outliers)
pg <- pg[pg_age > 0 | gillett== 1, rel_age:= pg_age]
pg <- pg[pg_age == 0 & gillett== 0, rel_age:= age]
pg <- pg[rel_age > 110, rel_age:= 0]

## remove two columns used to create the new age column
pg <- pg[, c("age","pg_age") := NULL]

## subset the data into planned giving donors and planned giving prospects
pgd <- pg[gillett==1,]
pgp <- pg[gillett==0,]

#### plot pgd

## age of planned gift
ggplot(pgd[rel_age > 0,], aes(x=rel_age)) + 
  geom_histogram(aes(y = ..density..), color="black", fill="light blue", binwidth = 1) +
  geom_density(color="blue", fill = "gold", alpha = 0.25) +
  scale_x_continuous(breaks = round(seq(min(pgd$rel_age), max(pgd$rel_age), by = 5),1)) +
  theme_bw()

## married?
ggplot(pgd, aes(factor(married))) + 
  geom_bar(fill="light blue", colour="black") +
  theme_bw()

## event count (need to revise for only pre-pg events)
ggplot(pgd, aes(x=event_count)) + 
  geom_histogram(aes(y = ..density..), color="black", fill="light blue", binwidth = 0.25) +
  geom_density(color="blue", fill = "gold", alpha = 0.25) +
  scale_x_continuous(breaks = round(seq(min(pgd$event_count), max(pgd$event_count), by = 0.25),2)) +
  theme_bw()

## kids 
ggplot(pgd, aes(factor(child_count))) + 
  geom_bar(fill="light blue", colour="black") +
  theme_bw()

## fys giving 
ggplot(pgd, aes(x=fys_giving)) + 
  geom_histogram(aes(y = ..density..), color="black", fill="light blue", binwidth = 5) +
  geom_density(color="blue", fill = "gold", alpha = 0.25) +
  scale_x_continuous(breaks = round(seq(min(pgd$fys_giving), max(pgd$fys_giving), by = 5),1)) +
  theme_bw()

## gift count
pgd <- pgd[, log_gc:= log(gift_count)]

ggplot(pgd, aes(x=log_gc)) + 
  geom_histogram(aes(y = ..density..), color="black", fill="light blue", binwidth = 0.5) +
  geom_density(color="blue", fill = "gold", alpha = 0.25) +
  scale_x_continuous(breaks = round(seq(min(pgd$log_gc), max(pgd$log_gc), by = 0.5),1)) +
  theme_bw()

## degree count 
ggplot(pgd[degree_count > 0,], aes(factor(degree_count))) + 
  geom_bar(fill="light blue", colour="black") +
  theme_bw()

## CAE type
ggplot(pgd, aes(factor(cae))) + 
  geom_bar(fill="light blue", colour="black") +
  theme_bw()

## constituent type
ggplot(pgd, aes(factor(const))) + 
  geom_bar(fill="light blue", colour="black") +
  theme_bw()

## const by CAE type
qplot(factor(cae), data=pgd, geom="bar", fill=factor(const))

## CAE by const type
qplot(factor(const), data=pgd, geom="bar", fill=factor(cae))
