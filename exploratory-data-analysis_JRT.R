
# Load packages
# (Install lpSolve package first)
library(mgcv)
library(Matrix)
library(lpSolve)
library(here)

#setwd("C:/Users/Muhannad/Desktop/multvariat/Time_series/mdp/Net-transition-probabilities-master")
# Read data
#


bmi.data<- read.csv(here::here("data", "ldl2.csv"),header=TRUE, sep=",")

#remove duplicated rows
bmi.data=bmi.data[!duplicated(bmi.data[ , c("SEQN")]),]

## Data clean ready to use

data_male= na.omit(bmi.data[,c(5,6,9,28,43)])
#data_male <- data_male[(data_male$LBXGLU>99.9 & data_male$LBXGLU<500.9 ),]
Type <- seq(1,length = nrow(data_male))
temp <- lapply(Type , function(x) ifelse(data_male$LBXGLU[x]<=125.9, "Pre","Diab") )
temp <- do.call(rbind,temp)
Type <- as.factor(temp)
rm(temp)
## error here
data_male <- cbind(data_male,Type)

### Creating factor for age
Age <-seq(1,length = nrow(data_male))
f<- function(x) { (x%/%10) }
temp <- lapply(Age , function(x) f(data_male$RIDAGEYR[x]) )
temp <- do.call(rbind, temp)
Age <- as.factor(temp)

data_male <- cbind(data_male,Age)


#####


###

###  Classifying Cholesterol Level.
Lip_Rati=data_male[,3]/data_male[,4]
data_male=cbind(data_male,Lip_Rati)
Chol=seq(1,length=nrow(data_male))
temp=lapply(Chol, function(x) ifelse(data_male$BMXBMI[x]<=25,"normal",ifelse(data_male$BMXBMI[x]<=30,"obes","ovrwieght")))
temp=do.call(rbind,temp)
Chol=as.factor(temp)
type=as.factor(temp)
data_male=cbind(data_male,Chol)

############  Graphing 

library(ggplot2)
#facet on race(RIDRETH1 )
fig <- ggplot(data_male , aes(x  = Age)) +
    geom_bar(aes( fill = type  ) , position ="fill") +
    scale_fill_viridis_d(end = 1.0) +
    facet_wrap(~ RIDRETH1)

fig
# Facet on statin use(TCrx)
fig <- ggplot(data_male , aes(x  = Age)) +
    geom_bar(aes( fill = type  ) , position ="fill") +
    scale_fill_viridis_d(end = 1.0) +
    facet_wrap(~TCRx )

fig



png(file = here::here("images", "exploratory.png"),
    res = 400, height = 9, width = 16, units = "in")
print(fig)
dev.off()

fig <- ggplot(data_male[data_male$Type=="Diab",] , aes(x  = Age)) + geom_bar(aes( fill = Chol  ) , position ="dodge" )
# print(fig + ggtitle("Diabetecs"))
# fig


library(tidyverse)
print(data_male %>%
          ggplot(aes(y = BMXBMI, x = type)) +
          geom_violin())


########################################################
bmi.data<- read.csv(here::here("data", "ldl2.csv"),header=TRUE, sep=",")
# generate data with non-constant variance
bmi.data=bmi.data[!duplicated(bmi.data[ , c("SEQN")]),]
dat=data.frame(bmi.data$RIDAGEYR,as.factor(bmi.data$RIDRETH1),as.factor(bmi.data$RIAGENDR),as.factor(bmi.data$TCRx),bmi.data$BMXBMI)
dat=na.omit(dat)

##
## use informative names!!!
##

colnames(dat) <- c("x","x2","x3","x4","y")
library(ggplot2)
ggplot(dat, aes(x,y)) + geom_point()
ggplot(dat, aes(x,y)) + geom_point() + geom_smooth(method="lm")
# quantile regression 
ggplot(dat, aes(x,y)) + 
    geom_point() + 
    geom_quantile(quantiles = 0.9)
library(quantreg)

library(splines)
X <- model.matrix(y ~ (bs(x, df = 4) + x2 + x3) * x4, data = dat)
qr1 <- rq(  y ~ X - 1, data=dat, tau = 0.9)
# qr1 <- rq(  y ~ x+x2+x3+x4 , data=dat, tau = 0.5)
summary(qr1) #lower bd and upper bd values are confidence intervals calculated using the "rank" method 

#Using the coef() function in combination with the geom_abline() function we can recreate
#what we got with geom_quantile() and ensure our results match:

ggplot(dat, aes(x,y)) + geom_point() + 
    geom_abline(intercept=coef(qr1)[1], slope=coef(qr1)[2], color = "red")


##
## Use predict to generate predicted lines
##
age_pred <- seq(16, 80, by = 1)
x2_pred <- factor(1:5)
x3_pred <- factor(1:2)
x4_pred <- factor(0:1)
dat_pred <- data.frame(expand.grid(age_pred, x2_pred, x3_pred, x4_pred))
names(dat_pred) <- c("x", "x2", "x3", "x4")

X_pred <- model.matrix(~ (bs(x, df = 4) + x2 + x3) * x4, data = dat_pred)

dat_prediction <- list(X = X_pred)
preds <- predict(qr1, newdata = dat_prediction, interval = "confidence")
dat_pred$preds  <- preds[, 1]
dat_pred$lower  <- preds[, 2]
dat_pred$higher <- preds[, 3]
dat_pred %>%
    ggplot(aes(x = x, y = preds, color = x4)) +
    geom_line() +
    geom_ribbon(aes(x = x, ymin = lower, ymax = higher, fill = x4), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7) +
    geom_point(data = dat, aes(x = x, y = y), alpha = 0.05) +
    facet_grid(x2 ~ x3)

##
## looks like we need a smooth term with age
##


contrasts(dat$x4)

 