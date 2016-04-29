library(data.table)
library(xgboost)
library(Matrix)
library(MLmetrics)
library(ggplot2)

setwd("C:/Users/pawlusm/Desktop")

## read in data set
gv <- fread("giving_vehicle.csv") 


## characters to integers
for (f in names(gv)) {
  if (class(gv[[f]])=="character") {
    levels <- unique(gv[[f]])
    gv[[f]] <- as.integer(factor(gv[[f]], levels=levels))
  }
}

## extract feature names (all columns except id and target)
feature.names <- names(gv)[c(3:ncol(gv))]

## sample some rows to split train and test
split <-sample(nrow(gv),nrow(gv)*0.5)

## make train and test
train <- gv[split,]
test <- gv[-split,]

#### one more split for a holdout set for xgboost
#h <-sample(nrow(train),nrow(train)*0.2)
#tra<-train[,feature.names, with=FALSE]  ## ( not needed? )

## extract target
target <- train[,method]

## create matrix
mtrain <- sparse.model.matrix(method ~ ., data = train[,names(train)[c(2:ncol(train))], with=FALSE])

## one more split for a holdout set for xgboost
nrow(mtrain)
h<-sample(nrow(mtrain),nrow(mtrain)*0.2) 

## create dense matrices for xgboost
dtrain <- xgb.DMatrix(data=mtrain[h,], label=target[h])
dval <- xgb.DMatrix(data=mtrain[-h,], label=target[-h])
watchlist <- list(val=dval,train=dtrain)

param <- list(  objective           = "multi:softprob",
                num_class           = 5,
                booster             = "gbtree",
                eta                 = 0.05,  # high value like 0.2 best result so far
                #max_depth           = 2,  # default of 6 is best so far
                subsample           = 0.7, # 0.9 working best
                colsample_bytree    = 0.7,
                #gamma               = 0.1,
                #alpha               = 0.001,
                #min_child_weight    = 2,
                #max_delta_step      = 0.5, # 1 is best so far
                eval_metric         = "mlogloss"
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 10000, 
                    verbose             = 2,
                    early.stop.round    = 50,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

# Get the feature real names
names <- dimnames(mtrain)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = clf)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

mtest <- sparse.model.matrix(method ~ ., data = test[,names(test)[c(2:ncol(test))], with=FALSE])

preds <- predict(clf, mtest)
preds = matrix(preds,5,length(preds)/5)
preds = t(preds)

pred <- as.data.frame(preds)
pred$id <- test$core_id
pred$target <- test$method

names(pred) <- c("int","person","mail","phone","online","id","target")

MultiLogLoss(pred$target, pred[,2:5])

head(pred[pred$target==1,], n=10)

head(pred[pred$target==2,], n=10)

head(pred[pred$target==3,], n=10)

head(pred[pred$target==4,], n=10)

gv <- fread("giving_vehicle.csv") 

ggplot(data=gv,aes(x=method)) + geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=-0.5)

ggplot(gv, aes(age, fill = as.factor(method), colour = as.factor(method))) +
  geom_density(alpha = 0.1) +
  xlim(20, 90)

ggplot(gv, aes(factor(type2), fill=factor(method)) ) +
  geom_bar(position="fill")

ggplot(gv, aes(factor(geo_dist), fill=factor(method)) ) +
  geom_bar(position="fill")

ggplot(gv, aes(factor(type1), fill=factor(method)) ) +
  geom_bar(position="fill")

ggplot(gv, aes(factor(p_sols), fill=factor(method)) ) +
  geom_bar(position="fill")

ggplot(gv, aes(factor(m_sols), fill=factor(method)) ) +
  geom_bar(position="fill")

ggplot(gv, aes(factor(e_sols), fill=factor(method)) ) +
  geom_bar(position="fill")

