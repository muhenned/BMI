
# Load packages
# (Install lpSolve package first)
library(mgcv)
library(Matrix)
library(here)
library(dplyr)
library(tidyverse)
library(quantreg)
library(splines)


bmi.data<- read.csv(here::here("data", "ldl2.csv"),header=TRUE, sep=",")

#remove duplicated rows and remove na data.
bmi.data=bmi.data[!duplicated(bmi.data[ , c("SEQN")]),]
bmi.data$LR=bmi.data$LBXTC/bmi.data$LBDHDD
bmi.data= na.omit(bmi.data[,c(5,6,9,28,43,32,48)]) 

colnames(bmi.data) <- c("Gender","Age","Race","BMI","Statin_status","Glu","LR") 
bmi.data= bmi.data[,c(1,2,3,5,6,7)]

bmi.data$Gender=recode(bmi.data$Gender, "1" = "Male", "2" = "Femal" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

bmi.data$Race=recode(bmi.data$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
# Classifying population to diabetes and non diabetes


Type <- seq(1,length = nrow(bmi.data))
temp <- lapply(Type , function(x) ifelse(bmi.data$Glu[x]<=125.9, "Pre","Diab") )
temp <- do.call(rbind,temp)
Type_glu <- as.factor(temp)
bmi.data <- cbind(bmi.data,Type_glu)


### Creating factor for age
Age_10 <-seq(1,length = nrow(bmi.data))
f<- function(x) { (x%/%10) }
temp <- lapply(Age_10 , function(x) f(bmi.data$Age[x]) )
temp <- do.call(rbind, temp)
Age_10 <- as.factor(temp)

bmi.data <- cbind(bmi.data,Age_10)



###  Classifying BMI Level.

BMI_bin=seq(1,length=nrow(bmi.data))
temp=lapply(BMI_bin, function(x) ifelse(bmi.data$BMI[x]<=25,"normal",ifelse(bmi.data$BMI[x]<=30,"obes","ovrwieght")))
temp=do.call(rbind,temp)
BMI__class=as.factor(temp)
#type=as.factor(temp)
bmi.data=cbind(bmi.data,BMI__class)

############  Graphing 

library(ggplot2)
#facet on race(RIDRETH1 )
fig <- ggplot(bmi.data , aes(x  = Age_10)) +
    geom_bar(aes( fill = BMI__class  ) , position ="fill") +
    scale_fill_viridis_d(end = 1.0) +
    facet_wrap(~ Race)

fig
# Facet on statin use(TCrx)
fig <- ggplot(bmi.data , aes(x  = Age_10)) +
    geom_bar(aes( fill = BMI__class  ) , position ="fill") +
    scale_fill_viridis_d(end = 1.0) +
    facet_wrap(~Statin_status )

fig



png(file = here::here("images", "exploratory.png"),
    res = 400, height = 9, width = 16, units = "in")
print(fig)
dev.off()


print(bmi.data %>%
          ggplot(aes(y = BMI, x = BMI__class)) +
          geom_violin())


########################################################

dat=data.frame(bmi.data$Age,as.factor(bmi.data$Race),as.factor(bmi.data$Gender),as.factor(bmi.data$Statin_status),bmi.data$LR,bmi.data$Type_glu)
colnames(dat) <- c("Age","Race","Gender","Statin_status","LR","Type_glu")
ggplot(dat, aes(y=LR  ,x=Age)) +
    geom_point()
ggplot(dat, aes(Age ,LR )) +
    geom_point() + 
    geom_smooth(method="lm")
##
#### quantile regression 
## 
fig3=ggplot(dat, aes(Age ,LR, group = Statin_status)) + 
    #geom_point() + 
    #geom_quantile(aes(color = Statin_status), quantiles = 0.8) +
    geom_quantile(aes(color = Statin_status), quantiles = 0.9) +
    #geom_quantile(aes(color = Statin_status),quantiles = 0.99) +
    facet_grid(Gender ~ Type_glu)+
    theme_bw()+
    ggtitle("90% LR Quatile")
png(file = here::here("images", "quant90_LR.png"),
    res = 400, height = 9, width = 16, units = "in")
print(fig3)
dev.off()



dat %>%
    group_by(Race, Gender, Statin_status) %>%
    summarize(count = n())


##
## Fit model to prediabetes sample
##

dat_pre <- dat %>% 
    filter(dat$Type_glu=="Pre")

X      <- model.matrix( LR ~ (bs(Age, df = 4) + Race + Gender ) * Statin_status, data = dat_pre)
qr_fit_pre_diabetes <- rq( LR ~ bs(Age, df = 4) + Race + Gender + Statin_status, data=dat_pre, tau = 0.9)

# qr1 <- rq(  y ~ x+x2+x3+x4 , data=dat, tau = 0.5)
summary(qr_fit_pre_diabetes) #lower bd and upper bd values are confidence intervals calculated using the "rank" method 



##
## Fit model to diabetes sample
##

dat_Diab=dat %>% filter(dat$Type_glu=="Diab")
# X <- model.matrix( BMI ~ (bs(Age, df = 4) + Race +Gender ) * Statin_status, data = dat_Diab)
# X <- model.matrix( BMI ~ bs(Age, df = 4) * (Race + Gender + Statin_status), data = dat_Diab)
X <- model.matrix( LR ~ bs(Age, df = 4) + Race + Gender + Statin_status, data = dat_Diab)
qr_fit_diabetes <- rq(  dat_Diab$LR ~ X - 1, data=dat_Diab, tau = 0.9)
summary(qr_fit_diabetes)
#Using the coef() function in combination with the geom_abline() function we can recreate
#what we got with geom_quantile() and ensure our results match:

ggplot(dat, aes(Age,LR)) + 
    geom_point() +
    geom_abline(intercept=coef(qr_fit_diabetes)[1], slope=coef(qr_fit_diabetes)[2], color = "red")


##
## Use predict to generate predicted lines
##
age_pred <- seq(16, 80, by = 1)
x2_pred <- factor(1:5)
x3_pred <- factor(1:2)
x4_pred <- factor(0:1)
dat_pred <- data.frame(expand.grid(age_pred, x2_pred, x3_pred, x4_pred))
names(dat_pred) <- c("Age", "Race", "Gender", "Statin_status")

dat_pred$Gender=recode(dat_pred$Gender, "1" = "Male", "2" = "Femal" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

dat_pred$Race=recode(dat_pred$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
# Classifying population to diabetes and non diabetes


#colnames(dat) <- c("Age","Race","Gender","Statin_status","BMI")
# X_pred <- model.matrix(~ (bs(x, df = 4) + Race + Gender) * Statin_status, data = dat_pred)
X_pred <- model.matrix(  ~ (bs(Age, df = 4) + Race + Gender) * Statin_status, data = dat_pred)



dat_prediction <- list(X = X_pred)
preds <- predict(qr_fit_pre_diabetes, newdata = dat_pred, interval = "confidence")
dat_pred$preds  <- preds[, 1]
dat_pred$lower  <- preds[, 2]
dat_pred$higher <- preds[, 3]

png(file = here::here("images", "exploratory2.png"),
    res = 400, height = 9, width = 16, units = "in")
fig2=dat_pred %>%
    ggplot(aes(x = Age, y = preds, color = Statin_status)) +
    geom_line() +
    geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7) +
    #geom_point(data = dat, aes(x = Age, y = LR), alpha = 0.05) +
    facet_grid(Race ~ Gender)+
    ylim(1, 7)
print(fig2)
dev.off()

##
## looks like we need a smooth term with age
##


