##required packages:
install.packages("boot")
library(boot)
install.packages("car")
library(car)
install.packages("QuantPsyc")
library(QuantPsyc)
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
install.packages("MASS")
library(MASS)
install.packages("nortest")
library(nortest)
install.packages("vars")
library(vars)

##setting working directory as Documents :
getwd()
setwd("~/")

##Reading dataset from working directory:
market <- read.csv("market.csv")

##changing variable names for our convenience:
names(market)<- c("marketid","marketsize","locationid","ageofstore","promotion","week","salesinthousands")
View(market)

##making a copy of Data set "original"
original <- market
View(original)

##checking for factor variables
str(market)
market$marketid <- as.factor(market$marketid)
market$week <- as.factor(market$week)
market$promotion <- as.factor(market$promotion)
str(market)

##outlier treatment for only numeric variables

boxplot(market$locationid) ##no outliers
boxplot(market$ageofstore)
quantile(market$ageofstore, c(0.01,0.05,0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.5,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,0.99))
market <- market[market$ageofstore<23,]
boxplot(market$ageofstore)
boxplot(market$salesinthousands)
quantile(market$salesinthousands, c(0.01,0.1,0.5,0.5,0.55,0.6,0.7,0.8,0.9,0.95,0.99))
market <- market[market$salesinthousands<78,]
boxplot(market$salesinthousands)

## fitting model in which we are considering all the
## variables and checking which variables are significant
## i.e checking statistically which explanatory variable(s)
## truely contributes to the response variable.

basemodel <- lm(salesinthousands~.,data = market)
summary(basemodel)

model1 <- lm(salesinthousands~marketid+locationid+ageofstore
             +promotion+week, data = market)

summary(model1)

model2 <- lm(salesinthousands~marketid+locationid+ageofstore
             +promotion, data = market)

summary(model2)

model3 <-lm(salesinthousands~marketid+ageofstore
            +promotion, data = market)
summary(model3)

model4 <- lm(salesinthousands~marketid
             +promotion, data = market)
summary(model4)

finalmodel <- lm(salesinthousands~ I(marketid==2)
                  +I(marketid==3)
                  +I(marketid==4)
                  +I(marketid==5)
                  +I(marketid==7)
                  +I(marketid==8)
                  +I(marketid==9)
                  +I(marketid==10)
                  +promotion, data = market)

summary(finalmodel)

names(finalmodel)

market$prediction <- fitted(finalmodel)

##checking multicollinearity

vif(finalmodel)

##checking Heteroscedasticity
bptest(finalmodel)

##checking Autocorrelation 

dwtest(finalmodel)
x <- finalmodel$residuals
hist(x)
ad.test(x)

actual <- market$salesinthousands
predicted <- market$prediction

##calculating mape
mape <- mean(abs((actual-predicted)/actual))
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                            bptest(model6)
##vif <- multicollinearity
##bptest <-  heteroscedasticity
##ad.test <- residuals are normally distributed or not
##dwttest <- autocorrelation
