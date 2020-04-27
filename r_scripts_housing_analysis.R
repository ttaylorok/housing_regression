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
plot(fit)

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
av <- aov(SalePrice ~ Neighborhood +
            MSZoning + 
            Street +
            LotShape +
            LandContour +
            Utilities +
            LotConfig +
            LandSlope +
            Condition1 +
            Condition2 +
            BldgType +
            HouseStyle +
            RoofStyle +
            RoofMatl +
            Exterior1st +
            Exterior2nd, data = data)
# Summary of the analysis
summary(av)

library(dplyr)

factors <- dplyr::select_if(data, is.factor)

factors2 <- factors[,c(-2,-3,-6,-29)]
factors3 <- factors2[,c(-36)]



omg <- cbind(data$SalePrice, factors3)

omg <- rename(omg,SalePrice = 'data$SalePrice')

av <- aov(formula(omg[,0:35]), data=omg)
summary(av)



omg <- rename(omg, SalePrice = "data$SalePrice") #For renaming dataframe column



columns(factors)
