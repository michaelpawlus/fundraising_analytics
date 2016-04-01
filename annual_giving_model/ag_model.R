## load libraries
library(ggplot2)
library(gridExtra)
library(readr)
#library(caret)  # not used but referenced
library(Matrix)
library(corrplot)
library(xgboost)
library(pROC)


## read in data
ag <- read_csv("ag_model.csv")

## or read in remotely
x <- getURL("https://raw.githubusercontent.com/michaelpawlus/fundraising_analytics/master/annual_giving_model/ag_model.csv")
pg <- read_csv(x)

## randomly sample half of the row indices to split training/test set
h<-sample(nrow(ag),nrow(ag)*0.5) 

## make train and test
train <- ag[h,]
test <- ag[-h,]

##### Removing IDs
train.id <- train$coreid
train$coreid <- NULL
test.id <- test$coreid
test$coreid <- NULL

##### Extracting TARGET
train.y <- train$target
train$target <- NULL
test.y <- test$target
test$target <- NULL

## ID and target or response (what you are trying to predict) as removed so they are not used in modeling

## define column type for plotting

## column names that refer to categories
cat.var.names <- c("degree","major","dept","post_direction","pre_direction","street_type","sub_addr","addr_flag1","addr_flag2","addr_flag3","addr_code2","addr_code4")

## column names that refer to continuous, ordinal numbers
cont.var.names <- c("gift_count","fys","first_fy","last_fy","act_yrs","unrestricted_count","unrestricted_percent","honmem_count","honmem_percent","event_count","vol_count","group_count","grad_yr","cons_giv_yrs","birth_year","birth_month","birth_day","median_income","annual_two")

## seperate categorical and continuous for plotting
train.cat <- train[, cat.var.names]
test.cat <- test[, cat.var.names]

train.cont <- train[, cont.var.names]
test.cont <- test[, cont.var.names]


## look at features

## this portion adapted from: https://www.kaggle.com/wittmaan/prudential-life-insurance-assessment/exploring-the-data

## for each of these plots you are looking for a pattern in repeat donors not seen in donors that do not give again the next year

## plot categorical as histograms

plotHist <- function(data.in, i) {
  data <- data.frame(x=data.in[,i])
  p <- ggplot(data=data, aes(x=factor(x), fill=as.factor(train.y))) + geom_histogram(position="dodge") + xlab(colnames(data.in)[i]) + theme_light() + 
    theme(axis.text.x=element_text(size=8))
  return (p)
}

doPlots <- function(data.in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data.in=data.in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

doPlots(data.in=train.cat, fun=plotHist, ii=1:4, ncol=2)

doPlots(data.in=train.cat, fun=plotHist, ii=5:8, ncol=2)

doPlots(data.in=train.cat, fun=plotHist, ii=9:12, ncol=2)

## plot continuous as density

plotDensity <- function(data.in, i) {
  data <- data.frame(x=data.in[,i])
  p <- ggplot(data=data, aes(x=x, colour=as.factor(train.y), group=as.factor(train.y))) + 
    geom_density(fill=NA) +
    xlab(colnames(data.in)[i]) + theme_light()
  return (p)
}

doPlots(data.in=train.cont, fun=plotDensity, ii=1:4, ncol=2)

doPlots(data.in=train.cont, fun=plotDensity, ii=5:8, ncol=2)

doPlots(data.in=train.cont, fun=plotDensity, ii=9:12, ncol=2)

doPlots(data.in=train.cont, fun=plotDensity, ii=13:16, ncol=2)

doPlots(data.in=train.cont, fun=plotDensity, ii=17:19, ncol=2)


## some plot may be better viewed individually so you can filter the data.

## here it is helpful to add the reponse variable back in and it is need for the last portion

train$target <- train.y

##Before plotting single plots you may have to use the line below
## if you see this error: Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state

dev.off()

## pick any from the grouped plots that had too much white space and add filters  (a few examples below)

ggplot(train[train$birth_year>1900,], aes(x=birth_year, colour=as.factor(target), group=as.factor(target))) + 
  geom_density(fill=NA) +
  theme_bw()

ggplot(train[train$first_fy>1960,], aes(x=first_fy, colour=as.factor(target), group=as.factor(target))) + 
  geom_density(fill=NA) +
  theme_bw()

ggplot(train[train$act_yrs<50,], aes(x=act_yrs, colour=as.factor(target), group=as.factor(target))) + 
  geom_density(fill=NA) +
  theme_bw()

ggplot(train[train$annual_two<1001,], aes(x=annual_two, colour=as.factor(target), group=as.factor(target))) + 
  geom_density(fill=NA) +
  theme_bw()


## code outliers in active years as NAs which is what they actually happen to be

train$act_yrs[train$act_yrs>50] <- -1

## the next two for loops are useful for removing unhelpful features but do not pertain to this dataset

## remove constant features
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}


## Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]

## there are a number of functions included in caret that are great for pre-processing as well
## I can't get caret to load at the moment
## here are some examples:  http://topepo.github.io/caret/preprocess.html

## make a correlation plot of your features

#compute the correlation matrix
descrCor_pear <-  cor(scale(train,center=TRUE,scale=TRUE), method="pearson")
descrCor_spea <-  cor(scale(train,center=TRUE,scale=TRUE), method="spearman")

#visualize the matrix, clustering features by correlation index.
corrplot(descrCor_pear, order = "hclust", mar=c(0,0,1,0), tl.pos="n", main="Pearson correlation")
corrplot(descrCor_spea, order = "hclust", mar=c(0,0,1,0), tl.pos="n", main="Spearman correlation")


## at this point, remove any features that don't seem helpful and finish any clean-up from what you have seen in the visualizations

## now, make a really simple model to check for feature importance

## I will use XGBoost but Random Forests has this too as well as most linear models


mtrain <- sparse.model.matrix(target ~ ., data = train)


nrow(mtrain)
h<-sample(nrow(mtrain),nrow(mtrain)*0.2)    # try different samples


dtrain <- xgb.DMatrix(data=mtrain[h,], label=train.y[h])
dval <- xgb.DMatrix(data=mtrain[-h,], label=train.y[-h])
watchlist <- list(val=dval,train=dtrain)


param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.005,
                max_depth           = 4,
                subsample           = 0.6,
                colsample_bytree    = 0.6,
                min_child_weight    = 18,
                gamma               = 0.65
)


clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 100000, 
                    verbose             = 2,
                    early.stop.round    = 50,
                    watchlist           = watchlist,
                    maximize            = TRUE
)

# Get the feature real names
names <- dimnames(mtrain)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = clf)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])


test$TARGET <- -1
mtest <- sparse.model.matrix(TARGET ~ ., data = test)

preds <- predict(clf, mtest)

auc(test.y, preds)


roc <- roc(test.y, preds)

plot.roc(roc)
