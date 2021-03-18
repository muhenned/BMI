
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


bmi.data= na.omit(data.frame(bmi.data$RIAGENDR,bmi.data$RIDAGEYR,bmi.data$RIDRETH1,bmi.data$BMXBMI,bmi.data$TCRx,bmi.data$LBXGLU)) 
colnames(bmi.data) <- c("Gender","Age","Race","BMI","Cholesterol_Drug_Use","Glu")
bmi.data=bmi.data[bmi.data$Age>19,]
#Recoding
#bmi.data$Cholesterol_Drug_Use=recode(bmi.data$Cholesterol_Drug_Use, "1" = "Yes", "0" = "No" )
bmi.data$Gender=recode(bmi.data$Gender, "1" = "Male", "2" = "Female" )
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


# Saving on object in RData format
save(bmi.data, file = "data.Rdata")


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
    facet_wrap(~Cholesterol_Drug_Use)

fig



png(file = here::here("images", "exploratory.png"),
    res = 400, height = 9, width = 16, units = "in")
print(fig)
dev.off()


print(bmi.data %>%
          ggplot(aes(y = BMI, x = BMI__class)) +
          geom_violin())


########################################################

dat=data.frame(bmi.data$Age,as.factor(bmi.data$Race),as.factor(bmi.data$Gender),as.factor(bmi.data$Cholesterol_Drug_Use),bmi.data$BMI,bmi.data$Type_glu)
colnames(dat) <- c("Age","Race","Gender","Cholesterol_Drug_Use","BMI","Type_glu")
dat$Cholesterol_Drug_Use=recode(dat$Cholesterol_Drug_Use, "1" = "Yes", "0" = "No" )
ggplot(dat, aes(y=BMI  ,x=Age)) +
    geom_point()
ggplot(dat, aes(Age ,BMI )) +
    geom_point() + 
    geom_smooth(method="lm")

################


##
# Box plot 

Age_10 <-seq(1,length = nrow(dat))
f<- function(x) { (x%/%10) }
temp <- lapply(Age_10 , function(x) f(bmi.data$Age[x]) )
temp <- do.call(rbind, temp)
Age_10 <- as.factor(temp)

dat <- cbind(dat,Age_10)
dat=na.omit(dat)

png(file = here::here("images", "base_box_plot.png"),
    res = 400, height = 14, width = 20, units = "in")

p<-ggplot(dat, aes(x=Age_10, y=BMI, color=Cholesterol_Drug_Use)) +
    geom_boxplot(notch = FALSE, fill = "lightgray" )+
    stat_summary(fun = mean, geom = "point",
                 shape = 18, size = 2.5, color = "#FC4E07")+
    ylim(c(18,60))+
    xlab("Age Group")+
    ylab("BMI")+
    theme_bw(base_size = 35)+
    facet_grid(Gender~Race)+
    labs(title ="  BMI for Colesterol drug users and who are not",subtitle =" ", caption = " NHANES data set")

print(p)
dev.off()







##
#### quantile regression 
## 
fig3=ggplot(dat, aes(Age ,BMI, group = Cholesterol_Drug_Use)) + 
    #geom_point() + 
    #geom_quantile(aes(color = Cholesterol_Drug_Use), quantiles = 0.8) +
    geom_quantile(aes(color =Cholesterol_Drug_Use), quantiles = 0.9) +
    #geom_quantile(aes(color = Cholesterol_Drug_Use),quantiles = 0.99) +
    facet_grid(Gender ~ Type_glu)+
    theme_bw()+
    ggtitle("90% Quatile")
png(file = here::here("images", "quant90.png"),
    res = 400, height = 9, width = 16, units = "in")
print(fig3)
dev.off()


##

######
##
## Use predict to generate predicted lines
dat=filter(dat,Statin_status!='9')
#levels(dat$Statin_status) <- c('1','2')
dat$Statin_status <- factor(dat$Cholesterol_Drug_Use)
X <- model.matrix( BMI ~ bs(Age, df = 4) + Race + Gender + Cholesterol_Drug_Use, data = dat)
qr_fit_dat <- rq(  dat$BMI ~ X - 1, data=dat, tau = 0.9)

##
age_pred <- seq(16, 80, by = 1)
x2_pred <- factor(1:5)
x3_pred <- factor(1:2)
x4_pred <- factor(0:1)
dat_pred <- data.frame(expand.grid(age_pred, x2_pred, x3_pred, x4_pred))
names(dat_pred) <- c("Age", "Race", "Gender", "dat$Cholesterol_Drug_Use")

dat_pred$Gender=recode(dat_pred$Gender, "1" = "Male", "2" = "Female" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

dat_pred$Race=recode(dat_pred$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
# Classifying population to diabetes and non diabetes


#colnames(dat) <- c("Age","Race","Gender","Statin_status","BMI")
# X_pred <- model.matrix(~ (bs(x, df = 4) + Race + Gender) * Statin_status, data = dat_pred)
X_pred <- model.matrix( ~ bs(Age, df = 4) + Race + Gender+ Cholesterol_Drug_Use, data = dat_pred)



dat_prediction <- list(X = X_pred)
preds <- predict(qr_fit_dat, newdata =dat_prediction, interval = "confidence")
dat_pred$preds  <- preds[, 1]
dat_pred$lower  <- preds[, 2]
dat_pred$higher <- preds[, 3]

png(file = here::here("images", "exploratory2.png"),
    res = 400, height = 9, width = 16, units = "in")
fig2=dat_pred %>%
    ggplot(aes(x = Age, y = preds, color = Cholesterol_Drug_Use)) +
    geom_line() +
    geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Cholesterol_Drug_Use), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7) +
    geom_point(data = dat, aes(x = Age, y = BMI), alpha = 0.05) +
    facet_grid(Type_glu ~ Gender)
print(fig2)
dev.off()










##
##############
###


dat %>%
    group_by(Race, Gender, Statin_status) %>%
    summarize(count = n())


##
## Fit model to prediabetes sample
##

dat_pre <- dat %>% 
    filter(dat$Type_glu=="Pre")

X      <- model.matrix( BMI ~ (bs(Age, df = 4) + Race + Gender ) * Statin_status, data = dat_pre)
qr_fit_pre_diabetes <- rq( BMI ~ bs(Age, df = 4) + Race + Gender + Statin_status, data=dat_pre, tau = 0.9)

# qr1 <- rq(  y ~ x+x2+x3+x4 , data=dat, tau = 0.5)
summary(qr_fit_pre_diabetes) #lower bd and upper bd values are confidence intervals calculated using the "rank" method 



##
## Fit model to diabetes sample
##

dat_Diab=dat %>% filter(dat$Type_glu=="Diab")
# X <- model.matrix( BMI ~ (bs(Age, df = 4) + Race +Gender ) * Statin_status, data = dat_Diab)
# X <- model.matrix( BMI ~ bs(Age, df = 4) * (Race + Gender + Statin_status), data = dat_Diab)
#X <- model.matrix( BMI ~ bs(Age, df = 4) + Race + Gender + Statin_status, data = dat_Diab)
qr_fit_diabetes <- rq(  dat_Diab$BMI ~ X - 1, data=dat_Diab, tau = 0.9)
summary(qr_fit_diabetes)
#Using the coef() function in combination with the geom_abline() function we can recreate
#what we got with geom_quantile() and ensure our results match:

ggplot(dat, aes(Age,BMI)) + 
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

dat_pred$Gender=recode(dat_pred$Gender, "1" = "Male", "2" = "Female" )
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
    ggplot(aes(x = Age, y = preds, color = Cholesterol_Drug_Use)) +
    geom_line() +
    geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Cholesterol_Drug_Use), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7) +
    geom_point(data = dat, aes(x = Age, y = BMI), alpha = 0.05) +
    facet_grid(Race ~ Gender)
print(fig2)
dev.off()

##
## looks like we need a smooth term with age



#############################################################################

plot(summary(rq(BMI~Cholesterol_Drug_Use+Type_glu+Race+Age+Gender,tau = 1:49/50,data=dat)))

plot(summary(rq(Glu~Cholesterol_Drug_Use+Race+Age+Gender,tau = 1:49/50,data=bmi.data ))) 

#############
##################
###########

l=bmi.data %>% subset(TCRx == 1) %>% count(RXDDRUG) %>% arrange(desc(n))
