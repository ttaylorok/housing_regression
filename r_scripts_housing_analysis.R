data = read.csv("DATA/train.csv")
as.numeric(data$FireplaceQu)

model.l = loess(SalePrice ~ GrLivArea,
                data = data,
                span = 0.75,        ### higher numbers for smoother fits
                degree=2,           ### use polynomials of order 2
                family="gaussian")  ### the default, use least squares to fit

library(rcompanion)
par(mar=c(1,1,1,1))
plotPredy(data  = data,
          x     = GrLivArea,
          y     = SalePrice,
          model = model.l)

hist(log(data$LotArea))
length(which(data$LotArea == 0))
sum(is.na(data$GrLivArea))

hist(data$LotFrontage)
length(which(data$LotFrontage == 0))
sum(is.na(data$LotFrontage))

hist(log(data$TotalBsmtSF))
length(which(data$TotalBsmtSF == 0))
sum(is.na(data$TotalBsmtSF))




#### MODEL 1

fit1 <- lm(SalePrice ~ GrLivArea, data=data)
summary(fit1)


# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit1)

##### MODEL 2

fit2 <- lm(SalePrice ~ GrLivArea + log(data$LotArea), data=data)
summary(fit2)

#### MODEL 3

fit3 <- lm(SalePrice ~ GrLivArea +
             log(LotArea) +
             TotalBsmtSF, data=data)
summary(fit3)

#### MODEL 4

fit4 <- lm(SalePrice ~ GrLivArea +
             log(LotArea) +
             TotalBsmtSF +
             OverallQual, data=data)
summary(fit4)

#### MODEL 5

fit5 <- lm(SalePrice ~ GrLivArea +
             log(LotArea) +
             TotalBsmtSF +
             OverallQual +
             X1stFlrSF, data=data)
summary(fit5)

#### MODEL 6

fit6 <- lm(SalePrice ~ GrLivArea +
             log(LotArea) +
             TotalBsmtSF +
             OverallQual +
             X1stFlrSF +
             TotRmsAbvGrd, data=data)
summary(fit6)

#### MODEL 7

fit7 <- lm(SalePrice ~ GrLivArea +
             log(LotArea) +
             TotalBsmtSF +
             OverallQual +
             X1stFlrSF +
             TotRmsAbvGrd +
             GarageArea, data=data)
summary(fit7)

#### MODEL 8

fit8 <- lm(SalePrice ~ GrLivArea +
             log(LotArea) +
             TotalBsmtSF +
             OverallQual +
             X1stFlrSF +
             TotRmsAbvGrd +
             GarageArea +
             GarageCars, data=data)
summary(fit8)

#### MODEL 9

fit9 <- lm(SalePrice ~ GrLivArea +
             log(LotArea) +
             TotalBsmtSF +
             OverallQual +
             X1stFlrSF +
             TotRmsAbvGrd +
             GarageArea +
             GarageCars +
             OverallCond, data=data)
summary(fit9)

#### MODEL 10

fit10 <- lm(SalePrice ~ GrLivArea +
             log(LotArea) +
             TotalBsmtSF +
             OverallQual +
             X1stFlrSF +
             TotRmsAbvGrd +
             GarageArea +
             GarageCars +
             OverallCond +
             YearBuilt, data=data)
summary(fit10)
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4),2,2))
plot(fit4)

# categorical
nl <- length(unique(data$Neighborhood))
fit11 <- lm(SalePrice ~ poly(as.numeric(Neighborhood),20),
            data = data)
summary(fit11)


nh <- aggregate(x = data$SalePrice, 
          by = list(data$Neighborhood), 
                      FUN = mean)

nhs = nh[order(nh$x),]

nums <- as.numeric(factor(data$Neighborhood, levels = nhs$Group.1, exclude = NULL))

data$nh_nums <- nums

# TRY DELETING OUTLIERS
data2 <- data[c(-1299,-524),]

fit12 <- lm(SalePrice ~ GrLivArea +
              log(LotArea) +
              TotalBsmtSF +
              OverallQual +
              X1stFlrSF +
              TotRmsAbvGrd +
              GarageArea +
              GarageCars +
              OverallCond +
              YearBuilt +
              nh_nums, data=data2)
summary(fit12)
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4),2,2))
plot(fit12)


# Compute the analysis of variance


library(dplyr)

factors <- dplyr::select_if(data, is.factor)

factors2 <- factors[,c(-2,-3,-6,-29)]
factors3 <- factors2[,c(-36)]

#factors4 <- subset(factors, -c(factors$Street, factors$Alley))

# exclude variables v1, v2, v3
dropvars <- names(factors) %in% c("Street", "Alley", "Utilities","CentralAir","PoolQC","Fence","MiscFeature")
omg2 <- factors[!dropvars]



omg3 <- cbind(data$SalePrice, omg2)

#omg4 <- rename(omg3, 'data$SalePrice' = SalePrice)

av <- aov(formula(omg3[,1:5]), data=omg3)
summary(av)

av2 <- aov(formula(omg3), data=omg3)
summary(av2)



#omg <- rename(omg, SalePrice = "data$SalePrice") #For renaming dataframe column

msz <- aggregate(x = data2$SalePrice, 
                by = list(data2$MSZoning), 
                FUN = mean)
msz <- msz[order(msz$x),]
nums <- as.numeric(factor(data2$MSZoning, levels = msz$Group.1, exclude = NULL))
data2$msz_nums <- nums

# remove factors with a low significance

fit13 <- lm(SalePrice ~ GrLivArea +
              log(LotArea) +
              TotalBsmtSF +
              OverallQual +
              TotRmsAbvGrd +
              GarageArea +
              OverallCond +
              YearBuilt +
              nh_nums +
              msz_nums, data=data2)
summary(fit13)
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4),2,2))
plot(fit13)


ord_numeric <- function(data,colx,coly)
{
  temp <- aggregate(x = data[,coly], 
                   by = list(data[,colx]), 
                   FUN = mean)
  temp <- temp[order(temp$x),]
  nums <- as.numeric(factor(data[,colx], levels = temp$Group.1, exclude = NULL))
  return(nums)
}

data2$ext_qual_num = ord_numeric(data=data2,colx='ExterQual',coly='SalePrice')
data2$sale_cond_num = ord_numeric(data=data2,colx='SaleCondition',coly='SalePrice')
data2$kitch_qual_num = ord_numeric(data=data2,colx='KitchenQual',coly='SalePrice')
data2$bsmt_qual_num = ord_numeric(data=data2,colx='BsmtQual',coly='SalePrice')
data2$roof_matl_num = ord_numeric(data=data2,colx='RoofMatl',coly='SalePrice')
data2$roof_stl_num = ord_numeric(data=data2,colx='RoofStyle',coly='SalePrice')
data2$bldg_type_num = ord_numeric(data=data2,colx='BldgType',coly='SalePrice')
data2$house_stl_num = ord_numeric(data=data2,colx='HouseStyle',coly='SalePrice')
data2$lot_shp_num = ord_numeric(data=data2,colx='LotShape',coly='SalePrice')
data2$lot_cnt_num = ord_numeric(data=data2,colx='LandContour',coly='SalePrice')

plot(data2$ext_qual_num, data2$SalePrice)

fit13 <- lm(SalePrice ~ GrLivArea +
              log(LotArea) +
              TotalBsmtSF +
              OverallQual +
              TotRmsAbvGrd +
              GarageArea +
              OverallCond +
              YearBuilt +
              nh_nums +
              ext_qual_num +
              sale_cond_num +
              kitch_qual_num +
              roof_matl_num +
              roof_stl_num +
              lot_cnt_num, data=data2)
summary(fit13)
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4),2,2))
plot(fit13)

#data3 <- data2[c(-692,-1183,-899),]

fit14 <- lm(SalePrice ~ GrLivArea +
              log(LotArea) +
              TotalBsmtSF +
              OverallQual +
              TotRmsAbvGrd +
              GarageArea +
              OverallCond +
              YearBuilt +
              nh_nums +
              ext_qual_num +
              sale_cond_num +
              kitch_qual_num +
              roof_matl_num +
              roof_stl_num +
              lot_cnt_num, data=data2)
summary(fit14)
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4),2,2))
plot(fit14)

#cd <- cooks.distance(fit14)



cooksd <- cooks.distance(fit14)

# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(data2)
layout(1,1,1)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

top_x_outlier <- 5
influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))
influential2 <- sort(cooksd, decreasing = TRUE, index.return=TRUE)$ix[1:top_x_outlier]

### MODEL 15, remove outliers

data3 <- data2[-influential2, ]

fit15 <- lm(SalePrice ~ GrLivArea +
              log(LotArea) +
              TotalBsmtSF +
              OverallQual +
              TotRmsAbvGrd +
              OverallCond +
              YearBuilt +
              nh_nums +
              ext_qual_num +
              sale_cond_num +
              kitch_qual_num +
              roof_stl_num +
              lot_cnt_num, data=data3)
summary(fit15)
par(mar=c(2,2,2,2))
layout(matrix(c(1,2,3,4),2,2))
plot(fit15)

#https://stackoverflow.com/questions/26237688/rmse-root-mean-square-deviation-calculation-in-r
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

rmse_fit15 = RMSE(fit15$fitted.values,data3$SalePrice)

library(tidyverse)
library(caret)
library(xgboost)

tb = as_tibble(data3)

features_train <- select(tb,'GrLivArea',
                         'LotArea',
                         'TotalBsmtSF',
                         'OverallQual',
                         'TotRmsAbvGrd',
                         'OverallCond',
                         'YearBuilt',
                         'nh_nums',
                         'ext_qual_num',
                         'sale_cond_num',
                         'kitch_qual_num',
                         'roof_stl_num','lot_cnt_num')
response_train <- select(tb, 'SalePrice')

######### RANDOM FOREST MODEL

library(randomForest)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data3), 0.7*nrow(data3), replace = FALSE)
TrainSet <- data3[train,]
ValidSet <- data3[-train,]
summary(TrainSet)
summary(ValidSet)

tunegrid <- expand.grid(
  mtry = c(2,4,6,8),
  ntree = c(50,100,200),
  nPerm = c(1,2,3),
  sampsize = c(nrow(data3), 0.8*nrow(data3), 0.6*nrow(data3)),
  replace = c(TRUE,FALSE)
)

control <- trainControl(method="repeatedcv",
                        number=5,
                        repeats=3,
                        search="grid")

rf_gridsearch1 <- train(x = as.matrix(features_train),
                       y = as_vector(response_train),
                       method="rf",
                       trControl=control)

data_test = read_csv("DATA/test.csv")

data_test_filt = select(j11,'GrLivArea',
                        'LotArea',
                        'TotalBsmtSF',
                        'OverallQual',
                        'TotRmsAbvGrd',
                        'OverallCond',
                        'YearBuilt',
                        'nh_nums',
                        'ext_qual_num',
                        'sale_cond_num',
                        'kitch_qual_num',
                        'roof_stl_num',
                        'lot_cnt_num')

qc <- data_test_filt %>% summarise_all(~sum(is.na(.)))

na_vals <- which(is.na(data_test_filt$TotalBsmtSF))

data_test_filt$TotalBsmtSF[na_vals] = 0

na_vals <- which(is.na(data_test_filt$kitch_qual_num))

data_test_filt$kitch_qual_num[na_vals] = 1


rf_pred <- predict(rf_gridsearch1,newdata=data_test_filt)

out_rf <- data.frame(cbind(data_test$Id, rf_pred))
write_csv(out_rf,"rf_model.csv")


customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree","nPerm","sampsize","replace"),
                                  class = rep("numeric", 5),
                                  label = c("mtry", "ntree","nPerm","sampsize","replace"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree = param$ntree,
               nPerm = param$nPerm,
               sampsize = param$sampsize,
               replace = param$replace)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Create a Random Forest model with default parameters
model1 <- randomForest(x = as.matrix(features_train),
                       y = as.matrix(response_train),
                       importance = TRUE)

control <- trainControl(method="repeatedcv",
                        number=5,
                        repeats=3,
                        search="grid")

#mtry <- sqrt(ncol(x))
#tunegrid <- expand.grid(.mtry=mtry)

start_time <- Sys.time()
rf_gridsearch <- train(x = as.matrix(features_train),
                       y = data3$SalePrice,
                       method=customRF,
                       metric="RMSE",
                       tuneGrid=tunegrid,
                       trControl=control)
end_time <- Sys.time()
end_time - start_time

for(i in 1:nrow(hyper_grid_rf)){
  print(i)
  rf.crossValidation(model1,
                     xdata = as.matrix(features_train),
                     ydata = as.matrix(response_train),
                     n = 5)
}


predTrain <- predict(model1, TrainSet, type = "class")
table(predTrain, TrainSet$SalePrice)



# Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "class")
# Checking classification accuracy
RMSE(predValid,ValidSet$SalePrice)                    
table(predValid,ValidSet$SalePrice)

mean(predValid == ValidSet$SalePrice)                    

table(predValid,ValidSet$SalePrice)





xgb.fit1 <- xgb.cv(
  data = as.matrix(features_train),
  label = as.matrix(response_train),
  nrounds = 100,
  nfold = 5,
  objective = "reg:linear",  # for regression models
  verbose = 0               # silent,
)

# get number of trees that minimize error
xgb.fit1$evaluation_log %>%
  dplyr::summarise(
    ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
    rmse.train   = min(train_rmse_mean),
    ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
    rmse.test   = min(test_rmse_mean),
  )

hyper_grid <- expand.grid(
  eta = c(.01, .05, .1, .3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(.65, .8, 1), 
  colsample_bytree = c(.8, .9, 1),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)


# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = as.matrix(features_train),
    label = as.matrix(response_train),
    nrounds = 100,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  #print(which.min(xgb.tune$evaluation_log$test_rmse_mean))
  #print(min(xgb.tune$evaluation_log$test_rmse_mean))
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)

hyper_grid$min_RMSE

# plot error vs number trees
ggplot(xgb.fit1$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")

# xgb_best <- xgb.train(x=as.matrix(features_train),
#                       y=as.matrix(response_train))

ps <- hyper_grid[which(hyper_grid$min_RMSE == min(hyper_grid$min_RMSE)),]

xg <- xgboost(data = as.matrix(features_train),
        label = as.matrix(response_train),
        params = list(eta = ps$eta,
                      max_depth = ps$max_depth,
                      min_child_weight = ps$min_child_weight,
                      subsample = ps$subsample,
                      colsample_bytree = ps$colsample_bytree),
        nrounds = 50)

xgpred <- predict(xg, newdata = as.matrix(data_test_filt))
out_xgb <- data.frame(cbind(data_test$Id, xgpred))
write_csv(out_xgb,"xgpred2_model.csv")

