setwd("C:/Users/pawlusm/Desktop")

library(readr)
library(data.table)
#library(dplyr)
#library(reshape2)
library(bit64)

pg <- fread("pg_prospects.csv")

## convert characters to integers
pg <- pg[, cae:=as.integer(as.factor(cae))]
pg <- pg[, const:=as.integer(as.factor(const))]

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

## create a vector of IDs to iterate over
ids <- pgp[,coreid]

## select a portion of this ID vector approx. equal to number of donor records (to force a balanced dataset)
ids.selected <- sample(x = ids,size = 200,replace = FALSE)

## subset the prospect pool using the IDs selected above
pgps <- pgp[coreid %in% ids.selected,]

## remove those IDs from the vector so they are not recycled
ids <- ids[!(ids %in% ids.selected)]

## create a new subset that is a combination of the full donor set and the prospect subset 
pg.sub <- rbind(pgd,pgps)

## create a vector of the IDs from the balanced subset
idx <- pg.sub[,coreid]

## create a train and test set from here (hopefully each will have about a 50/50 donor:prospect ratio)
pg.train <- pg.sub[coreid %in% sample(x = idx,size = 195,replace = FALSE),]
pg.test <- pg.sub[coreid %in% sample(x = idx,size = 200,replace = FALSE),]


## train some models and predict for train and test
## (using glm for convenience -- used summary() to decide on final features retained for the model)
fit <- glm(gillett ~ gift_count + fys_giving + act_consistency + 
            child_count + event_count + rel_age, data=pg.train, family=binomial())
pred.test <- predict(fit, pg.test, type = "response")

fit <- glm(gillett ~ gift_count + fys_giving + act_consistency + 
             child_count + event_count + rel_age, data=pg.test, family=binomial())
pred.train <- predict(fit, pg.train, type = "response")

## place the predictions in a new column in each data table
pg.train$pred <- pred.train
pg.test$pred <- pred.test

## create a combined data table
pg.comb <- rbind(pg.train,pg.test)

## subset just the prospects
pg.prospects <- pg.comb[gillett==0,]

## place the prospects in a new data table to create a container for the iterative results created by the "for loop"
pg.all.prospects <- pg.prospects



## cycle over your the remained of your dataset n times to ensure all or almost all IDs are passed to the model

#### I'm sure there is a better way to do this (please tell me)

#### Also, I am getting duplicate IDs in my final complete prospect prediction set (does anyone know why?)


for (i in 1:80) {
  
  ids.selected <- sample(x = ids,size = 200,replace = FALSE)
  
  pgps <- pgp[coreid %in% ids.selected,]
  
  ids <- ids[!(ids %in% ids.selected)]
  
  pg.sub <- rbind(pgd,pgps)
  
  idx <- pg.sub[,coreid]
  
  pg.train <- pg.sub[coreid %in% sample(x = idx,size = 195,replace = FALSE),]
  pg.test <- pg.sub[coreid %in% sample(x = idx,size = 200,replace = FALSE),]
  
  fit <- glm(gillett ~ gift_count + fys_giving + act_consistency + 
               child_count + event_count + rel_age, data=pg.train, family=binomial())
  pred.test <- predict(fit, pg.test, type = "response")
  
  fit <- glm(gillett ~ gift_count + fys_giving + act_consistency + 
               child_count + event_count + rel_age, data=pg.test, family=binomial())
  pred.train <- predict(fit, pg.train, type = "response")
  
  pg.train$pred <- pred.train
  pg.test$pred <- pred.test
  
  pg.comb <- rbind(pg.train,pg.test)
  
  pg.prospects <- pg.comb[gillett==0,]
  
  ## all new predeictions are added to existing predictions
  pg.all.prospects <- rbind(pg.all.prospects,pg.prospects)
  
  
}

## write the final prediction file
write_csv(pg.all.prospects, "pg_prospects1.csv")




