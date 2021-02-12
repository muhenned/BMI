#Marginal effect on BMI with splines
#Plot for the confidence interval of BMI regressed on splines of Age, 
#race, gender,and cholesterol medications use.
 
library(quantreg)
library(splines)
library(tidyverse)

#memory.limit(size=56000)
#attach(bmi.data)
dat=readRDS(file = "bmi.data.rds")
#dat=dat[dat$BMI<28,]
dat=filter(dat,Statin_status!='9')
levels(dat$Statin_status) <- c('1','2')
dat$Statin_status <- factor(dat$Statin_status)
X <- model.matrix( BMI ~ bs(Total_chol, df = 5) + Race + Gender + Statin_status, data = dat)
qr_fit_dat <- rq(  dat$BMI ~ X - 1, data=dat, tau = 0.9)

 
#formula <- Glu ~Gender+Race+BMI+Statin_status+bs(Age,intercept=FALSE,df=5)+bs(Total_chol,intercept=FALSE,df=5)+bs(waist_cir,intercept=FALSE,df=5)
# formula <- Glu ~ Gender+Race+Statin_status+bs(Age,intercept=FALSE,df=5)#+bs(Total_chol,intercept=FALSE,df=5)+bs(waist_cir,intercept=FALSE,df=5)      

  
 
 
#model <- rq(Glu ~ bs(Age, knots=c(25,50,75))+BMI+Gender, tau=0.5, data=bmi.data ) 

# qr_fit_dat <- rq(formula, tau=0.5, data=bmi.data ) 
 
 
#plot
#age_pred <- seq(16, 80, by = 1)
x2_pred <- factor(1:5)
x3_pred <- factor(1:2)
x4_pred <- factor(1:2)
x5_pred <-seq(min(dat$Total_chol),400,by=1)
dat_pred <- data.frame(expand.grid(age_pred, x2_pred, x3_pred, x4_pred))
names(dat_pred) <- c( "Race", "Gender", "Statin_status","Total_chol" )

dat_pred$Gender=recode(dat_pred$Gender, "1" = "Male", "2" = "Femal" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

dat_pred$Race=recode(dat_pred$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
# Classifying population to diabetes and non diabetes


#colnames(dat) <- c("Age","Race","Gender","Statin_status","BMI")
# X_pred <- model.matrix(~ (bs(x, df = 4) + Race + Gender) * Statin_status, data = dat_pred)
X_pred <- model.matrix( ~ bs(Total_chol, df = 5) + Race + Gender+ Statin_status , data = dat_pred)



dat_prediction <- list(X = X_pred)
pred <- predict(qr_fit_dat, newdata =dat_prediction, interval = "confidence")
dat_pred$pred  <- pred[, 1]
dat_pred$lower  <- pred[, 2]
dat_pred$higher <- pred[, 3]

# Get model-based predictions
 
    
###############################################################################4
fig2=dat_pred %>%
    ggplot(aes(x = Total_chol, y = pred, color = Statin_status)) +
    #geom_line() +
    geom_quantile(formula = y ~ bs(x,intercept=FALSE,df=5), quantiles = 0.9)+
    geom_ribbon(aes(x = Total_chol, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    ylim(25,50)+
    scale_fill_viridis_d(end = 0.7)#+
    #facet_grid(Race~Statin_status)
    
fig2





################################################################################################

#create data    
x <- seq(0,100,length.out = 100)        
sig <- 0.1 + 0.05*x 
b_0 <- 6                                
b_1 <- 0.1                              
set.seed(1)                             
e <- rnorm(100,mean = 0, sd = sig)      
y <- b_0 + b_1*x + e 

mydata <- data.frame(x,y, age=sample(30:70,100,replace=TRUE), sex=sample(c("Male","Female"),100, replace=TRUE))

#run regression
library(quantreg)
library(splines)
model <- rq(y ~ ns(x, knots=c(25,50,75))+age+sex, tau=0.5, data=mydata ) 

#plot
#sp <- c(25,50,75)
#ggplot(mydata, aes(x=x,y=y))+ geom_point()+ geom_quantile(formula=y~ns(x,knots=sp), quantiles=0.5, se=T)


# Get model-based predictions
pred <- as.data.frame(predict(model, data.frame(x = mydata$x, age = mydata$age, sex = mydata$sex), interval = "confidence"));
pred$x <- mydata$x;

#plot
sp <- c(25,50,75);
ggplot(mydata, aes(x=x,y=y)) +
    geom_point() +
     #geom_line(data = pred, aes(x = x, y = fit)) +
    geom_ribbon(data = pred, aes(ymin = lower, ymax = higher, x = x), alpha = 0.4) +
    geom_quantile(formula = y ~ ns(x, knots = sp), quantiles = 0.8);
