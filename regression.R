library(tidyverse)
#library(moments) # skewness and kurtosis
library(caret)
library(ggplot2)
library(reshape2)
library(plotly)
library(e1071) # box-cox transform
library(RANN) # knn imputation
library(randomForest)

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

factor_filt2_sale <- cbind(SalePrice = data$SalePrice, factor_filt2)

########## DO SOME SMART ENCODING
summary(factor_filt2_sale)
# ordinal variables
ExterQual_tbl <- data.frame(ExterQual = c('Po','Fa','TA','Gd','Ex'),
                            ExterQual_val = c(1,2,3,4,5))
ExterCond_tbl <- data.frame(ExterCond = c('Po','Fa','TA','Gd','Ex'),
                            ExterCond_val = c(1,2,3,4,5))
BsmtQual_tbl <- data.frame(BsmtQual = c('None','Po','Fa','TA','Gd','Ex'),
                           BsmtQual_val = c(0,1,2,3,4,5))
BsmtExposure_tbl <- data.frame(BsmtQual = c('None','No','Mn','Av','Gd'),
                               BsmtQual_val = c(0,1,2,3,4))
HeatingQC_tbl <- data.frame(HeatingQC = c('Po','Fa','TA','Gd','Ex'),
                            HeatingQC_val = c(1,2,3,4,5))
KitchenQual_tbl <- data.frame(KitchenQual = c('None','Po','Fa','TA','Gd','Ex'),
                              KitchenQual_val = c(1,2,3,4,5,6))
FireplaceQu_tbl <- data.frame(FireplaceQu = c('None','Po','Fa','TA','Gd','Ex'),
                                FireplaceQu_val = c(0,1,2,3,4,5))
GarageCond_tbl <- data.frame(GarageCond = c('None','Po','Fa','TA','Gd','Ex'),
                            GarageCond_val = c(0,1,2,3,4,5))
PoolQC_tbl <- data.frame(PoolQC = c('None','Fa','TA','Gd','Ex'),
                         PoolQC_val = c(0,1,2,3,4))
# GarageFinish_tbl <- data.frame(GarageFinish = c('None','Unf','RFn','Fn'),
#                                GarageFinish_val = c(0,1,2,3))
# Functional_tbl <- data.frame(Functional = c('Sal','Sev','Maj2','Maj1','Mod','Min2','Min1','Typ'),
#                              Functional_val = c(1,2,3,4,5,6,7,8))

MSZoning_tbl <- factor_filt2_sale[,c('SalePrice','MSZoning')] %>%
  group_by(MSZoning) %>% summarise(meanSalePrice = mean(SalePrice))
MSZoning_tbl <- MSZoning_tbl %>%
  transmute(MSZoning = MSZoning, MSZoning_val = dense_rank(meanSalePrice))

Alley_tbl <- factor_filt2_sale[,c('SalePrice','Alley')] %>%
  group_by(Alley) %>% summarise(meanSalePrice = mean(SalePrice))
Alley_tbl <- Alley_tbl %>%
  transmute(Alley = Alley, Alley_val = dense_rank(meanSalePrice))

LotShape_tbl <- factor_filt2_sale[,c('SalePrice','LotShape')] %>%
  group_by(LotShape) %>% summarise(meanSalePrice = mean(SalePrice))
LotShape_tbl <- LotShape_tbl %>%
  transmute(LotShape = LotShape, LotShape_val = dense_rank(meanSalePrice))

LandContour_tbl <- factor_filt2_sale[,c('SalePrice','LandContour')] %>%
  group_by(LandContour) %>% summarise(meanSalePrice = mean(SalePrice))
LandContour_tbl <- LandContour_tbl %>%
  transmute(LandContour = LandContour, LandContour_val = dense_rank(meanSalePrice))

LotConfig_tbl <- factor_filt2_sale[,c('SalePrice','LotConfig')] %>%
  group_by(LotConfig) %>% summarise(meanSalePrice = mean(SalePrice))
LotConfig_tbl <- LotConfig_tbl %>%
  transmute(LotConfig = LotConfig, LotConfig_val = dense_rank(meanSalePrice))

Neighborhood_tbl <- factor_filt2_sale[,c('SalePrice','Neighborhood')] %>%
  group_by(Neighborhood) %>% summarise(meanSalePrice = mean(SalePrice))
Neighborhood_tbl <- Neighborhood_tbl %>%
  transmute(Neighborhood = Neighborhood, Neighborhood_val = dense_rank(meanSalePrice))

Condition1_tbl <- factor_filt2_sale[,c('SalePrice','Condition1')] %>%
  group_by(Condition1) %>% summarise(meanSalePrice = mean(SalePrice))
Condition1_tbl <- Condition1_tbl %>%
  transmute(Condition1 = Condition1, Condition1_val = dense_rank(meanSalePrice))

Condition2_tbl <- factor_filt2_sale[,c('SalePrice','Condition2')] %>%
  group_by(Condition2) %>% summarise(meanSalePrice = mean(SalePrice))
Condition2_tbl <- Condition2_tbl %>%
  transmute(Condition2 = Condition2, Condition2_val = dense_rank(meanSalePrice))

BldgType_tbl <- factor_filt2_sale[,c('SalePrice','BldgType')] %>%
  group_by(BldgType) %>% summarise(meanSalePrice = mean(SalePrice))
BldgType_tbl <- BldgType_tbl %>%
  transmute(BldgType = BldgType, BldgType_val = dense_rank(meanSalePrice))

HouseStyle_tbl <- factor_filt2_sale[,c('SalePrice','HouseStyle')] %>%
  group_by(HouseStyle) %>% summarise(meanSalePrice = mean(SalePrice))
HouseStyle_tbl <- HouseStyle_tbl %>%
  transmute(HouseStyle = HouseStyle, HouseStyle_val = dense_rank(meanSalePrice))

RoofStyle_tbl <- factor_filt2_sale[,c('SalePrice','RoofStyle')] %>%
  group_by(RoofStyle) %>% summarise(meanSalePrice = mean(SalePrice))
RoofStyle_tbl <- RoofStyle_tbl %>%
  transmute(RoofStyle = RoofStyle, RoofStyle_val = dense_rank(meanSalePrice))

RoofMatl_tbl <- factor_filt2_sale[,c('SalePrice','RoofMatl')] %>%
  group_by(RoofMatl) %>% summarise(meanSalePrice = mean(SalePrice))
RoofMatl_tbl <- RoofMatl_tbl %>%
  transmute(RoofMatl = RoofMatl, RoofMatl_val = dense_rank(meanSalePrice))

Exterior1st_tbl <- factor_filt2_sale[,c('SalePrice','Exterior1st')] %>%
  group_by(Exterior1st) %>% summarise(meanSalePrice = mean(SalePrice))
Exterior1st_tbl <- Exterior1st_tbl %>%
  transmute(Exterior1st = Exterior1st, Exterior1st_val = dense_rank(meanSalePrice))

Exterior2nd_tbl <- factor_filt2_sale[,c('SalePrice','Exterior2nd')] %>%
  group_by(Exterior2nd) %>% summarise(meanSalePrice = mean(SalePrice))
Exterior2nd_tbl <- Exterior2nd_tbl %>%
  transmute(Exterior2nd = Exterior2nd, Exterior2nd_val = dense_rank(meanSalePrice))

MasVnrType_tbl <- factor_filt2_sale[,c('SalePrice','MasVnrType')] %>%
  group_by(MasVnrType) %>% summarise(meanSalePrice = mean(SalePrice))
MasVnrType_tbl <- MasVnrType_tbl %>%
  transmute(MasVnrType = MasVnrType, MasVnrType_val = dense_rank(meanSalePrice))

Foundation_tbl <- factor_filt2_sale[,c('SalePrice','Foundation')] %>%
  group_by(Foundation) %>% summarise(meanSalePrice = mean(SalePrice))
Foundation_tbl <- Foundation_tbl %>%
  transmute(Foundation = Foundation, Foundation_val = dense_rank(meanSalePrice))

BsmtFinType1_tbl <- factor_filt2_sale[,c('SalePrice','BsmtFinType1')] %>%
  group_by(BsmtFinType1) %>% summarise(meanSalePrice = mean(SalePrice))
BsmtFinType1 <- BsmtFinType1_tbl %>%
  transmute(BsmtFinType1 = BsmtFinType1, BsmtFinType1_val = dense_rank(meanSalePrice))

Heating_tbl <- factor_filt2_sale[,c('SalePrice','Heating')] %>%
  group_by(Heating) %>% summarise(meanSalePrice = mean(SalePrice))
Heating_tbl <- Heating_tbl %>%
  transmute(Heating = Heating, Heating_val = dense_rank(meanSalePrice))

CentralAir_tbl <- factor_filt2_sale[,c('SalePrice','CentralAir')] %>%
  group_by(CentralAir) %>% summarise(meanSalePrice = mean(SalePrice))
CentralAir_tbl <- CentralAir_tbl %>%
  transmute(CentralAir = CentralAir, CentralAir_val = dense_rank(meanSalePrice))

GarageType_tbl <- factor_filt2_sale[,c('SalePrice','GarageType')] %>%
  group_by(GarageType) %>% summarise(meanSalePrice = mean(SalePrice))
GarageType_tbl <- GarageType_tbl %>%
  transmute(GarageType = GarageType, GarageType_val = dense_rank(meanSalePrice))

SaleType_tbl <- factor_filt2_sale[,c('SalePrice','SaleType')] %>%
  group_by(SaleType) %>% summarise(meanSalePrice = mean(SalePrice))
SaleType_tbl <- SaleType_tbl %>%
  transmute(SaleType = SaleType, SaleType_val = dense_rank(meanSalePrice))

j1 <- left_join(factor_filt2_sale, ExterQual_tbl)
j2 <- left_join(j1, ExterCond_tbl)
j3 <- left_join(j2, BsmtQual_tbl)
j4 <- left_join(j3, BsmtExposure_tbl)
j5 <- left_join(j4, HeatingQC_tbl)
j6 <- left_join(j5, KitchenQual_tbl)
j7 <- left_join(j6, FireplaceQu_tbl)
j8 <- left_join(j7, GarageCond_tbl)
j9 <- left_join(j8, PoolQC_tbl)
j10 <- left_join(j9, MSZoning_tbl)
j11 <- left_join(j10, Alley_tbl)
j12 <- left_join(j11, LotShape_tbl)
j13 <- left_join(j12, LandContour_tbl)
j14 <- left_join(j13, LotConfig_tbl)
j15 <- left_join(j14, Neighborhood_tbl)
j16 <- left_join(j15, Condition1_tbl)
j17 <- left_join(j16, Condition2_tbl)
j18 <- left_join(j17, BldgType_tbl)
j19 <- left_join(j18, RoofStyle_tbl)
j20 <- left_join(j19, RoofMatl_tbl)
j21 <- left_join(j20, Exterior1st_tbl)
j22 <- left_join(j21, Exterior2nd_tbl)
j23 <- left_join(j22, MasVnrType_tbl)
j24 <- left_join(j23, Heating_tbl)
j25 <- left_join(j24, CentralAir_tbl)
j26 <- left_join(j25, GarageType_tbl)
j27 <- left_join(j26, SaleType_tbl)

fact_cols <- grep('val', names(j27), value=TRUE)

dummies <- dummyVars(SalePrice ~ ., data = factor_filt2_sale)

dv <- predict(dummies, newdata = factor_filt2_sale)

nzv(dv, saveMetrics= TRUE)


sff <- c(num_zeros = ~sum(. == 0))
sumtable <- ttsummary(as_tibble(dv), sff)

# combine numerical and factor predictors

processed <- cbind(pp_trans, dv)
processed2 <- cbind(pp_trans, j27[,fact_cols])
fitdata_f1 <- cbind(SalePrice = pp_trans_y,processed2)
fitdata_f2 <- fitdata_f1[c(-1299,-524),]

ggplot(data = fitdata_f2, aes(x = as.factor(Neighborhood_val), y = SalePrice)) +
  geom_boxplot()

ggplot(data = fitdata_f2, aes(x = as.factor(ExterQual_val), y = SalePrice)) +
  geom_boxplot()

ggplot(data = fitdata_f2, aes(x = as.factor(MoSold), y = SalePrice)) +
  geom_boxplot()

########## LINEAR MODEL

fitdata1 <- cbind(SalePrice = pp_trans_y,pp_trans)
lmFit <- lm(SalePrice ~ ., data=fitdata1)
summary(lmFit)

cooksd <- cooks.distance(lmFit)
sample_size <- nrow(pp_trans)
layout(matrix(1))
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

fitdata2 <- fitdata1[c(-1299,-524),]

ctrl <- safsControl(functions = caretSA, verbose = TRUE,
                    ## 10-fold CV
                    method = "repeatedcv",
                    number = 10,
                    repeats = 1)
obj100 <- safs(x = data.frame(pp_trans), 
            y = as_vector(pp_trans_y),
            iters = 100,
            safsControl = ctrl,
            ## Now pass options to `train`
            method = "lm")

obj100v2 <- safs(x = fitdata2[which(colnames(fitdata2) != 'SalePrice')], 
               y = fitdata2$SalePrice,
               iters = 100,
               safsControl = ctrl,
               ## Now pass options to `train`
               method = "lm")

plot(obj100v2) + theme_bw()

finalCols <- obj100v2[["fit"]][["finalModel"]][["xNames"]]

predict(obj100v2,fitdata2[which(colnames(fitdata2) != 'SalePrice')])

ctrlGA <- gafsControl(functions = caretGA, verbose = TRUE,
                    ## 10-fold CV
                    method = "repeatedcv",
                    number = 10,
                    repeats = 1)
gafsFit1 <- gafs(x = fitdata2[which(colnames(fitdata2) != 'SalePrice')], 
                 y = fitdata2$SalePrice,
                 iters = 4,
                 gafsControl = ctrlGA,
                 ## Now pass options to `train`
                 method = "lm")

rfe_ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 1,
                   verbose = TRUE)
subsets <- seq(from = 5, to = 60, by = 5)
lmProfile <- rfe(x = fitdata_f1[which(colnames(fitdata_f1) != 'SalePrice')],
                 y = fitdata_f1$SalePrice,
                 sizes = subsets,
                 rfeControl = rfe_ctrl)

#names(fitdata_f2) <- 1:length(names(fitdata_f2))
#names(processed) <- 1:length(names(processed))
lmProfile <- rfe(x = data.frame(processed),
                 y = as.matrix(pp_trans_y),
                 sizes = subsets,
                 rfeControl = rfe_ctrl)

plot(lmProfile)

lmProfile2 <- rfe(x = data.frame(fitdata_f2[,-1]),
                 y = as_vector(fitdata_f2[,1]),
                 sizes = subsets,
                 rfeControl = rfe_ctrl)
plot(lmProfile2)

SA_fit2 <- safs(x = data.frame(fitdata_f2[,-1]), 
                y = as_vector(fitdata_f2[,1]),
                 iters = 200,
                 safsControl = ctrl,
                 ## Now pass options to `train`
                 method = "lm")

plot(SA_fit2) + theme_bw()


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


gbmFit2 <- train(SalePrice ~ ., data = fitdata_f2, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit2

gbmPred2 <- predict(gbmFit2,fitdata_f2)
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



tunegrid <- expand.grid(
  mtry = c(2,4,6,8,16,20,30,40),
  ntree = c(50,100,200,250,300),
  nPerm = c(1,2,3),
  sampsize = c(ceiling(nrow(fitdata_f2)*.9),ceiling(nrow(fitdata_f2)*.8),ceiling(nrow(fitdata_f2)*.6)),
  replace = c(TRUE,FALSE)
)
tunegrid <- expand.grid(
  mtry = c(20),
  ntree = c(300,400,500,600,800,1000,1200,1400),
  nPerm = c(1),
  sampsize = c(ceiling(nrow(fitdata_f2)*.8)),
  replace = c(FALSE)
)



customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree","nPerm","sampsize","replace"),
                                  class = c(rep("numeric", 4),"logical"),
                                  label = c("mtry", "ntree","nPerm","sampsize","replace"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree,
               nPerm=param$nPerm, sampsize = param$sampsize, replace = param$replace, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Create a Random Forest model with default parameters
# model1 <- randomForest(x = data.frame(fitdata_f2[,-1]),
#                        y = as_vector(fitdata_f2[,1]),
#                        importance = TRUE)

control <- trainControl(method="repeatedcv",
                        number=10,
                        repeats=1,
                        search="grid")

#mtry <- sqrt(ncol(x))
#tunegrid <- expand.grid(.mtry=mtry)

rf_gridsearch2 <- train(x = data.frame(fitdata_f2[,-1]),
                       y = as_vector(fitdata_f2[,1]),
                       method=customRF,
                       metric="RMSE",
                       tuneGrid=tunegrid,
                       trControl=control)

rf_gridsearch2

xgControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 3)

# good workflow here:
# https://www.kaggle.com/pelkoja/visual-xgboost-tuning-with-caret
grid1 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(3, 4, 5, 6, 8),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

xgbFit1 <- caret::train(
  x = fitdata_f2[,-1],
  y = fitdata_f2[,1],
  trControl = xgControl,
  tuneGrid = grid1,
  method = "xgbTree",
  verbose = TRUE
)

xgbFit1

grid2 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 50),
  eta = xgbFit1$bestTune$eta,
  max_depth = xgbFit1$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = c(1, 2, 3),
  subsample = c(0.5, 0.75, 1.0)
)

xgbFit2 <- caret::train(
  x = fitdata_f2[,-1],
  y = fitdata_f2[,1],
  trControl = xgControl,
  tuneGrid = grid2,
  method = "xgbTree",
  verbose = TRUE
)



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

lmSA_Pred_out <- predict(obj100v2,pp_trans_test)
lmSA_Pred_inv_out <- inverse.BoxCoxTrans(pp_params_y$bc$SalePrice, lmSA_Pred_out)
outdata <- cbind(Id = data_test$Id, SalePrice = lmSA_Pred_inv_out)
write_csv(data.frame(outdata), 'lmSA_with_caret_v1.csv')


t_elec <- table(data_test$Electrical)
r_elec <- names(which(t_elec == max(t_elec)))

t_msn <- table(data_test$MasVnrType)
r_msn <- names(which(t_msn == max(t_msn)))

t_kit <- table(data_test$KitchenQual)
r_kit <- names(which(t_kit == max(t_kit)))

t_ext1 <- table(data_test$Exterior1st)
r_ext1 <- names(which(t_ext1 == max(t_ext1)))

t_ext2 <- table(data_test$Exterior2nd)
r_ext2 <- names(which(t_ext2 == max(t_ext2)))

t_sal <- table(data_test$SaleType)
r_sal <- names(which(t_sal == max(t_sal)))

t_msz <- table(data_test$MSZoning)
r_msz <- names(which(t_msz == max(t_msz)))

data_test <- replace_na(data_test, list(Electrical = r_elec,
                                            MasVnrType = r_msn,
                                        KitchenQual = r_kit,
                                        Exterior1st = r_ext1,
                                        Exterior2nd = r_ext2,
                                        SaleType = r_sal,
                                        MSZoning = r_msz))

levels(data_test$Alley) <- c(levels(data_test$Alley),"None")
levels(data_test$BsmtQual) <- c(levels(data_test$BsmtQual),"None")
levels(data_test$BsmtCond) <- c(levels(data_test$BsmtCond),"None")
levels(data_test$BsmtExposure) <- c(levels(data_test$BsmtExposure),"None")
levels(data_test$BsmtFinType1) <- c(levels(data_test$BsmtFinType1),"None")
levels(data_test$BsmtFinType2) <- c(levels(data_test$BsmtFinType2),"None")
levels(data_test$FireplaceQu) <- c(levels(data_test$FireplaceQu),"None")
levels(data_test$GarageType) <- c(levels(data_test$GarageType),"None")
levels(data_test$GarageFinish) <- c(levels(data_test$GarageFinish),"None")
levels(data_test$GarageQual) <- c(levels(data_test$GarageQual),"None")
levels(data_test$GarageCond) <- c(levels(data_test$GarageCond),"None")
levels(data_test$PoolQC) <- c(levels(data_test$PoolQC),"None")
levels(data_test$Fence) <- c(levels(data_test$Fence),"None")
levels(data_test$MiscFeature) <- c(levels(data_test$MiscFeature),"None")

data_test <- replace_na(data_test, list(Alley = "None", BsmtQual = "None",
                                            BsmtCond = "None", BsmtExposure = "None",
                                            BsmtFinType1 = "None", BsmtFinType2 = "None",
                                            FireplaceQu = "None", GarageType = "None",
                                            GarageFinish = "None", GarageQual = "None",
                                            GarageCond = "None", PoolQC = "None",
                                            Fence = "None", MiscFeature = "None"))

j1 <- left_join(data_test, ExterQual_tbl)
j2 <- left_join(j1, ExterCond_tbl)
j3 <- left_join(j2, BsmtQual_tbl)
j4 <- left_join(j3, BsmtExposure_tbl)
j5 <- left_join(j4, HeatingQC_tbl)
j6 <- left_join(j5, KitchenQual_tbl)
j7 <- left_join(j6, FireplaceQu_tbl)
j8 <- left_join(j7, GarageCond_tbl)
j9 <- left_join(j8, PoolQC_tbl)
j10 <- left_join(j9, MSZoning_tbl)
j11 <- left_join(j10, Alley_tbl)
j12 <- left_join(j11, LotShape_tbl)
j13 <- left_join(j12, LandContour_tbl)
j14 <- left_join(j13, LotConfig_tbl)
j15 <- left_join(j14, Neighborhood_tbl)
j16 <- left_join(j15, Condition1_tbl)
j17 <- left_join(j16, Condition2_tbl)
j18 <- left_join(j17, BldgType_tbl)
j19 <- left_join(j18, RoofStyle_tbl)
j20 <- left_join(j19, RoofMatl_tbl)
j21 <- left_join(j20, Exterior1st_tbl)
j22 <- left_join(j21, Exterior2nd_tbl)
j23 <- left_join(j22, MasVnrType_tbl)
j24 <- left_join(j23, Heating_tbl)
j25 <- left_join(j24, CentralAir_tbl)
j26 <- left_join(j25, GarageType_tbl)
j27 <- left_join(j26, SaleType_tbl)

#z<-j27[,fact_cols]
data_test_comb <- cbind(data_test['Id'],as_tibble(j27[,fact_cols]),pp_trans_test)

lmFR_facts_Pred_out <- predict(lmProfile2,data_test_comb)
lmFR_facts_Pred_out_inv <- inverse.BoxCoxTrans(pp_params_y$bc$SalePrice, lmFR_facts_Pred_out)
outdata <- cbind(Id = data_test['Id'], SalePrice = lmFR_facts_Pred_out_inv)
write_csv(data.frame(outdata), 'lmRF_factors_with_caret_v1.csv')





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
