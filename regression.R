library(tidyverse)
#library(moments) # skewness and kurtosis
library(caret)
library(ggplot2)
library(reshape2)
library(plotly)
library(e1071) # box-cox transform
library(RANN) # knn imputation

options(tibble.print_max = 40)

data = read_csv("DATA/train.csv")

ttsummary <- function(data, funcs){
  st <- as_tibble(names(data))
  for (i in 1:length(funcs)){
    tmp <- t(summarise_all(data, funcs[[i]]))[,1]
    st <- add_column(st, tmp, .name_repair = "unique")
  }
  names(st) <- append("column", names(funcs))
  return(st)
}

numeric_vars <- select_if(data[,which(colnames(data) != 'SalePrice' &
                                 colnames(data) != 'Id')], is.numeric)

########## CREATE SUMMARY TABLE

sf <- c(mean = ~mean(.,na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        num_zeros = ~sum(. == 0, na.rm =TRUE),
        skew = ~skewness(na.omit(.)),
        kurt = ~kurtosis(., na.rm = TRUE),
        nans = ~sum(is.na(.)))


sumtable <- ttsummary(numeric_vars, sf)

# good plotting ideas

########## TRANSFORM VALUES

ggplot(data = data, aes(SalePrice)) +
  geom_histogram(alpha=0.9, aes(y=..density..)) +
  geom_density() 

ggplot(data, aes(sample = SalePrice)) +
  stat_qq() + stat_qq_line()

########## CREATE CORRELATION TABLE

descrCor <-  cor(numeric_vars)

melted_cormat = melt(descrCor)

ct <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

fig <- ggplotly(ct)
#fig.write_html("test.html")
htmlwidgets::saveWidget(fig,'ct.html')

hist(numeric_vars$LotArea)

########## PREPROCESSING

#### NUMERICAL VARS

# for printing
nearZeroVar(numeric_vars, saveMetrics= TRUE)

# for applying
nzv <- nearZeroVar(numeric_vars, saveMetrics= FALSE)

filt_nzv <- numeric_vars[, -nzv]

#NOTE: KNN IMPUTATION WILL APPLY CENTERING AND SCALING
pp_params <- preProcess(data.frame(filt_nzv), method=c("BoxCox","knnImpute"))
pp_params_y <- preProcess(data.frame(SalePrice = data$SalePrice), method=c("BoxCox"))

pp_trans <- predict(pp_params, data.frame(filt_nzv))
pp_trans_y <- predict(pp_params_y, data.frame(SalePrice = data$SalePrice))

comboInfo <- findLinearCombos(pp_trans)

#hist(pp_trans$SalePrice)
hist(pp_trans$MSSubClass)
hist(pp_trans$GrLivArea)
hist(pp_trans$WoodDeckSF)

ppsum <- ttsummary(pp_trans, sf)

#### FACTOR VARS

# decide what factors to remove
data_fact <- data %>%  mutate_if(sapply(data, is.character), as.factor)
factor_vars <- select_if(data_fact, is.factor)

summary(factor_vars)

t_elec <- table(factor_vars$Electrical)
r_elec <- names(which(t_elec == max(t_elec)))

t_msn <- table(factor_vars$MasVnrType)
r_msn <- names(which(t_msn == max(t_msn)))

factor_filt <- replace_na(factor_vars, list(Electrical = r_elec,
                                            MasVnrType = r_msn))

levels(factor_filt$Alley) <- c(levels(factor_filt$Alley),"None")
levels(factor_filt$BsmtQual) <- c(levels(factor_filt$BsmtQual),"None")
levels(factor_filt$BsmtCond) <- c(levels(factor_filt$BsmtCond),"None")
levels(factor_filt$BsmtExposure) <- c(levels(factor_filt$BsmtExposure),"None")
levels(factor_filt$BsmtFinType1) <- c(levels(factor_filt$BsmtFinType1),"None")
levels(factor_filt$BsmtFinType2) <- c(levels(factor_filt$BsmtFinType2),"None")
levels(factor_filt$FireplaceQu) <- c(levels(factor_filt$FireplaceQu),"None")
levels(factor_filt$GarageType) <- c(levels(factor_filt$GarageType),"None")
levels(factor_filt$GarageFinish) <- c(levels(factor_filt$GarageFinish),"None")
levels(factor_filt$GarageQual) <- c(levels(factor_filt$GarageQual),"None")
levels(factor_filt$GarageCond) <- c(levels(factor_filt$GarageCond),"None")
levels(factor_filt$PoolQC) <- c(levels(factor_filt$PoolQC),"None")
levels(factor_filt$Fence) <- c(levels(factor_filt$Fence),"None")
levels(factor_filt$MiscFeature) <- c(levels(factor_filt$MiscFeature),"None")

factor_filt <- replace_na(factor_filt, list(Alley = "None", BsmtQual = "None",
                                            BsmtCond = "None", BsmtExposure = "None",
                                            BsmtFinType1 = "None", BsmtFinType2 = "None",
                                            FireplaceQu = "None", GarageType = "None",
                                            GarageFinish = "None", GarageQual = "None",
                                            GarageCond = "None", PoolQC = "None",
                                            Fence = "None", MiscFeature = "None"))

summary(factor_filt)

factor_filt_sale <- cbind(SalePrice = data$SalePrice, factor_filt)

av <- aov(SalePrice ~ ., data=factor_filt_sale, projections = TRUE)
summary(av)

## EXAMPLES OF STATISTICALLY SIGNIFICANT
# good thing we didn't filter out pool
ggplot(data = factor_filt_sale, aes(x = PoolQC, y = SalePrice)) +
  geom_boxplot()

ggplot(data = factor_filt_sale, aes(x = MSZoning, y = SalePrice)) +
  geom_boxplot()

## EXAMPLES OF INSIGNIFICANT
ggplot(data = factor_filt_sale, aes(x = BsmtFinType2, y = SalePrice)) +
  geom_boxplot()

ggplot(data = factor_filt_sale, aes(x = Fence, y = SalePrice)) +
  geom_boxplot()

ggplot(data = factor_filt_sale, aes(x = Electrical, y = SalePrice)) +
  geom_boxplot()

ggplot(data = factor_filt_sale, aes(x = Utilities, y = SalePrice)) +
  geom_boxplot()

# remove insignificant columns
av_pr <- summary(av)[[1]][["Pr(>F)"]]

factor_filt2 <- factor_filt[which(av_pr < 0.01)]

factor_filt2_sale <- cbind(SalePrice = data$SalePrice, factor_filt)

dummies <- dummyVars(SalePrice ~ ., data = factor_filt2_sale)

dv <- predict(dummies, newdata = factor_filt2_sale)


# combine numerical and factor predictors

processed <- cbind(pp_trans, dv)

########## LINEAR MODEL

fitdata1 <- cbind(SalePrice = pp_trans_y,pp_trans)
lmFit <- lm(SalePrice ~ ., data=fitdata1)
summary(lmFit)

par(mar=c(2,2,2,2))
layout(matrix(c(1,2,3,4),2,2))
plot(lmFit)

cooksd <- cooks.distance(lmFit)
sample_size <- nrow(pp_trans)
layout(matrix(1))
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
cooksd[1299]

#fitdata <- pp_trans[which(names(pp_trans) != "Id")]
fitdata2 <- fitdata1[c(-1299,-524),]

lmFit2 <- lm(SalePrice ~ ., data=fitdata2)
summary(lmFit2)

fitdata3 <- select(fitdata2, c(SalePrice,LotArea,OverallQual,OverallCond,YearBuilt,
                                  YearRemodAdd,BsmtFinSF1,TotalBsmtSF,
                                  X1stFlrSF, X2ndFlrSF, GrLivArea,BsmtFullBath,
                                  BedroomAbvGr,GarageCars,YrSold))

lmFit3 <- lm(SalePrice ~ ., data=fitdata3)
summary(lmFit3)

########## COMPARE AGAINST ORIGINAL

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

inverse.BoxCoxTrans <- function(object, newdata) {
  if(!is.vector(newdata) || !is.numeric(newdata)) stop("newdata should be a numeric vector")
  if(is.na(object$lambda)) return(newdata) 
  
  lambda <- object$lambda
  if(lambda < object$fudge & lambda > -object$fudge)
    lambda <- 0
  else if(lambda < 1+object$fudge & lambda > 1-object$fudge)
    lambda <- 1
  
  if(lambda == 0) exp(newdata) else (lambda*newdata + 1)^(1/lambda) 
}

#uc <- fitdata3$SalePrice*pp_params_y$std + pp_params_y$mean
inv_bc <- inverse.BoxCoxTrans(pp_params_y$bc$SalePrice, uc)

rmse_fit3 = RMSE(lmFit3$fitted.values,fitdata3$SalePrice)


########## GBM MODEL

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 3)

# test effect of imputation only
#pp_params2 <- preProcess(data.frame(filt_nzv), method=c("knnImpute"))
#pp_trans2 <- predict(pp_params2, data.frame(filt_nzv))

set.seed(825)
gbmFit1 <- train(SalePrice ~ ., data = fitdata2, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

gbmPred <- predict(gbmFit1,fitdata3)
#uc_gbm <- gbmPred*pp_params_y$std['SalePrice'] + pp_params_y$mean['SalePrice']
#inv_bc_gbm <- inverse.BoxCoxTrans(pp_params_y$bc$SalePrice, gbmPred)
RMSE(fitdata3$SalePrice, gbmPred)


rf1 <- train(SalePrice ~ ., data = fitdata3, 
                 method = "rf", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
rf1

refPred <- predict(rf1,fitdata3)
#uc_rf <- refPred*pp_params_y$std['SalePrice'] + pp_params_y$mean['SalePrice']
#inv_bc_rf <- inverse.BoxCoxTrans(pp_params_y$bc$SalePrice, uc_rf)
RMSE(fitdata3$SalePrice, refPred)










########## MAKE PREDICTIONS
data_test = read_csv("DATA/test.csv")
#cols_
numeric_vars_test <- select(data_test, colnames(filt_nzv))
pp_trans_test <- predict(pp_params, newdata = data.frame(numeric_vars_test))
#pp_trans_test_y <- predict(pp_params_y, newdata = data.frame(SalePrice = data_test$S))
rfPred_out <- predict(rf1,pp_trans_test)
#uc_rf_out <- rfPred_out*pp_params_y$std['SalePrice'] + pp_params_y$mean['SalePrice']
inv_bc_rf_out <- inverse.BoxCoxTrans(pp_params_y$bc$SalePrice, rfPred_out)
#RMSE(fitdata_filt$SalePrice, inv_bc_rf)
outdata <- cbind(Id = data_test$Id, SalePrice = inv_bc_rf_out)
write_csv(data.frame(outdata), 'rf_with_caret_v3.csv')

gbmPred_out <- predict(gbmFit1,pp_trans_test)
gbmPred_inv_out <- inverse.BoxCoxTrans(pp_params_y$bc$SalePrice, gbmPred_out)
outdata <- cbind(Id = data_test$Id, SalePrice = gbmPred_inv_out)
write_csv(data.frame(outdata), 'gbm_with_caret_v1.csv')

lmPred_out <- predict(lmFit3,pp_trans_test)
lmPred_inv_out <- inverse.BoxCoxTrans(pp_params_y$bc$SalePrice, lmPred_out)
outdata <- cbind(Id = data_test$Id, SalePrice = lmPred_inv_out)
write_csv(data.frame(outdata), 'lm_with_caret_v1.csv')









#boxplot(factor_filt$PoolQC, factor_filt$SalePrice)

factor_filt <- replace_na(factor_filt, list(Alley = "None"))

t <- table(factor_filt$MasVnrType)
tm <- names(which(t == max(t)))
factor_filt <- replace_na(factor_filt, list(MasVnrType = tm))

levels(factor_filt$Alley) <- c(levels(factor_filt$Alley),"None")
factor_filt <- replace_na(factor_filt, list(Alley = "None"))

recode_factor(filt_factors$Electrical, .missing = "M")
recode(char_vec, NA = "Apple")

summary(new$Electrical)

new <- replace_na(filt_factors, list(Electrical = 'SBrkr'))

# remove factor with only one level
filt_factors <- select(factor_vars,-c(Utilities, Street, PoolQC, Alley, Fence, MiscFeature))

table(filt_factors$BsmtQual)
na.replace(filt_factors$Electrical, .na="new")

new <- filt_factors %>% replace_na(list(Electrical = 'None'))

replace_na(Electrical = replace_na(Electrical, 0)))

filt_factors$Electrical <- replace_na(data = filt_factors$Electrical, 'None')

summary(new$Electrical)

sff <- c(nans = ~sum(is.na(.)))

fsumtable <- ttsummary(factor_vars, sff)

count(factor_vars,Alley)
count(factor_vars,Street)
count(factor_vars,Utilities)
count(factor_vars,CentralAir)
count(factor_vars,Fence)
count(factor_vars,PoolQC)
count(factor_vars,SaleCondition)

names(filt_factors)[37]
table(factor_vars)

sff <- c(nans = ~sum(is.na(.)))

sumtable <- ttsummary(factor_vars, sff)





predict(dummies, factor_vars)

unique(filt_factors$BsmtQual)




# TODO
# inverse box/cox?
# contained in http://inferenceandinduction.blogspot.com/2015/09/caret-boxcox-and-fudge.html
