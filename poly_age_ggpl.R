#Marginal effect on BMI with polynomial
#Plot for the confidence interval of BMI regressed on polynomial of Age, 
#race, gender,and cholesterol medications use.

library(quantreg)
library(splines)
library(tidyverse)
library(ggeffects)
memory.limit(size=560000)
#attach(bmi.data)
dat=readRDS(file = "bmi.data.rds")
dat <- cbind(dat, Age2 = dat$Age^2)
#dat=dat[dat$BMI<28,]
dat=filter(dat,Statin_status!='9')
levels(dat$Statin_status) <- c('1','2')
dat$Statin_status <- factor(dat$Statin_status)
X <- model.matrix( BMI ~ Gender+Race+Statin_status+Age+Age2 , data = dat)
qr_fit_dat <- rq(  dat$BMI ~ X - 1, data=dat, tau = 0.9)
#formula <- BMI ~Gender+Race+Statin_status+Age+Age2+Total_chol+Total_chol2

#formula <- Glu ~Gender+Race+BMI+Statin_status+bs(Age,intercept=FALSE,df=5)+bs(Total_chol,intercept=FALSE,df=5)+bs(waist_cir,intercept=FALSE,df=5)
# formula <- Glu ~ Gender+Race+Statin_status+bs(Age,intercept=FALSE,df=5)#+bs(Total_chol,intercept=FALSE,df=5)+bs(waist_cir,intercept=FALSE,df=5)      




#model <- rq(Glu ~ bs(Age, knots=c(25,50,75))+BMI+Gender, tau=0.5, data=bmi.data ) 

# qr_fit_dat <- rq(formula, tau=0.5, data=bmi.data ) 


#plot
age_pred <- seq(16, 80, by = 1)
x2_pred <- factor(1:5)
x3_pred <- factor(1:2)
x4_pred <- factor(1:2)
#x5_pred <-seq(min(dat$Total_chol),400,by=1)
dat_pred <- data.frame(expand.grid(age_pred,age_pred^2, x2_pred, x3_pred, x4_pred))
names(dat_pred) <- c( "Age","Age2","Race", "Gender", "Statin_status" )

dat_pred$Gender=recode(dat_pred$Gender, "1" = "Male", "2" = "Femal" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

dat_pred$Race=recode(dat_pred$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
# Classifying population to diabetes and non diabetes


#colnames(dat) <- c("Age","Race","Gender","Statin_status","BMI")
#X_pred <- model.matrix(~ (bs(x, df = 4) + Race + Gender) * Statin_status, data = dat_pred)
X_pred <- model.matrix( ~ Gender+Race+Statin_status+Age+Age2 , data = dat_pred)

X_pred=X_pred[c(1:100),]

dat_prediction <- list(X = X_pred)
pred <- predict(qr_fit_dat, newdata =dat_prediction, interval = "confidence")
dat_pred$pred  <- pred[, 1]
dat_pred$lower  <- pred[, 2]
dat_pred$higher <- pred[, 3]

# Get model-based predictions


###############################################################################4
fig2=dat_pred %>%
    ggplot(aes(x = Age, y = pred, color = Statin_status)) +
    geom_line() +
    #How to compute age effects
    #geom_quantile(formula = y ~ bs(x,intercept=FALSE,df=5), quantiles = 0.9)+
    #geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    #ylim(c(20,50))+
    theme_bw(base_size = 8)+
    facet_grid(Race~Gender)

fig2





################################################################################################


























##############################
 
fit <- rq(BMI ~ bs(Age,df=4)+Race+Gender+Statin_status,0.9 ,data = dat)
#fit <- rq(BMI ~ Age+Race+Gender+Statin_status+Total_chol,0.5 ,data = dat)
ggpredict(fit, terms = "Age")
 

library(ggplot2)
mydf <- ggpredict(fit, terms ="Age")
mydf=cbind(mydf,dat)
ggplot(mydf, aes(x, y=predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
    #geom_quantile(formula = y ~ , quantiles = c(0.2))+
    #geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    #ylim(c(20,50))+
    theme_bw(base_size = 8)+
    facet_grid(Race~Gender)

