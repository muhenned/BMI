 
#Marginal effect on BMI with splines
#Plot for the confidence interval of BMI regressed on splines of Age, 
#race, gender,and cholesterol medications use.


library(quantreg)
library(splines)
library(tidyverse)
library(ggeffects)

#memory.limit(size=56000)
#attach(bmi.data)
dat=readRDS(file = "bmi.data.rds")
#dat=dat[dat$BMI<28,]
dat=filter(dat,Statin_status!='9')
#levels(dat$Statin_status) <- c('1','2')
dat$Statin_status <- factor(dat$Statin_status)
dat$Age=as.numeric(dat$Age)

#formula 1
#formula=BMI ~ bs(Age, df = 5) + Race + Gender + Statin_status+Total_chol
#formula 2
#formula=BMI ~ bs(Total_chol, df=5)+ Race + Gender + Statin_status
# formula 3 
#formula=BMI ~ Age+Age^2+Total_chol+ Total_chol^2+ Race + Gender + Statin_status


fit_rq=function(dat,index,tau){
    if (index==1) {formula=BMI ~ bs(Age, df = 3) + Race + Gender + Statin_status}
    else if(index ==2) {formula=BMI ~ bs(Total_chol, df=5)+ Race + Gender + Statin_status}
    else {formula=BMI ~ Age+Age^2+Total_chol+ Total_chol^2+ Race + Gender + Statin_status}
    X <- model.matrix(formula,data=dat)
    qr_fit_dat <- rq( BMI ~ X - 1, data=dat, tau = tau)
    return(qr_fit_dat)
}

#index= 1 bsplines age and total cholesterol, Index=2 bspline total choles, 
#and index=3 polynomial age and total choles.
fit=fit_rq(dat,index=1,tau=0.9)





#plot
dat_pred_ready=function(dat,index,tau){
    age_pred <- seq(16, 80, by = 1)
    x2_pred <- factor(1:5)
    x3_pred <- factor(1:2)
    x4_pred <- factor(0:1)
    x5_pred <-seq(min(dat$Total_chol),400,by=1)
    if(index==1){
        dat_pred <- data.frame(expand.grid(age_pred, x2_pred, x3_pred, x4_pred))
        names(dat_pred) <- c( "Age","Race", "Gender", "Statin_status")
    }else if(index==2){
        dat_pred <- data.frame(expand.grid(x5_pred, x2_pred, x3_pred, x4_pred))
        names(dat_pred) <- c("Total_chol", "Race", "Gender", "Statin_status" )
    }else {
        dat_pred <- data.frame(expand.grid(age_pred, x2_pred, x3_pred, x4_pred,x5_pred))
        names(dat_pred) <- c( "Age","Race", "Gender", "Statin_status","Total_chol" )
    }
    
    
    dat_pred$Gender=recode(dat_pred$Gender, "1" = "Male", "2" = "Female" )
    
    
    dat_pred$Race=recode(dat_pred$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
    
    if(index==1){
        X_pred <- model.matrix( ~ bs(Age, df = 3) + Race + Gender+ Statin_status , data = dat_pred)
        
    }else if(index==2){
        X_pred <- model.matrix(~ bs(Total_chol, df=5)+ Race + Gender + Statin_status,data=dat_pred)
    }else {
        X_pred <- model.matrix(~Age+Age^2+Total_chol+ Total_chol^2+ Race + Gender + Statin_status,data=dat_pred)
    }
    
    
    fit=fit_rq(dat,index,tau)
    
    dat_prediction <- list(X = X_pred)
    pred <- predict(fit, newdata =dat_prediction, interval = "confidence")
    dat_pred$pred  <- pred[, 1]
    dat_pred$lower  <- pred[, 2]
    dat_pred$higher <- pred[, 3]
    return(dat_pred)
}

dat_pred=dat_pred_ready(dat,index =2 ,tau =0.9 )
# Get model-based predictions


###############################################################################4
fig2=dat_pred %>%
    ggplot(aes(x = Age, y = pred, color = Statin_status)) +
    geom_line() +
    #geom_quantile(formula = y ~ bs(x,intercept=FALSE,df=5), quantiles = 0.25)+
    geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    #ylim(c(20,70))+
    theme_bw(base_size = 8)+
    facet_grid(Race~Gender)

fig2

###################

# plot for splines on total cholestrol
fig2=dat_pred %>%
    ggplot(aes(x = Total_chol, y = pred, color = Statin_status)) +
    geom_line() +
    #geom_quantile(formula = y ~ bs(x,intercept=FALSE,df=5), quantiles = 0.25)+
    #geom_ribbon(aes(x =Total_chol  , ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    ylim(c(30,70))+
    theme_bw(base_size = 8)+
    facet_grid(Race~Gender)

fig2



################################################################################################

fit <- rq(BMI ~ bs(Age,df=4)+Race+Gender+Statin_status+bs(Total_chol,df=4),0.9 ,data = dat)
#fit <- rq(BMI ~ Age+Race+Gender+Statin_status+Total_chol,0.5 ,data = dat)



library(ggplot2)
mydf <- ggpredict(fit, terms ="Total_chol[all]")
mydf=cbind(mydf,dat)
ggplot(mydf, aes(x, y=predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
    #geom_quantile(formula = y ~bs(Total_chol,df=4) , quantiles = c(0.2))+
    #geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    #ylim(c(20,50))+
    theme_bw(base_size = 8)#+
#facet_grid(Race~Gender)

#######################################################



fit <- rq(BMI ~ bs(Age,df=4)+Race+Gender+Statin_status+Total_chol,0.1 ,data = dat)
#fit <- rq(BMI ~ Age+Race+Gender+Statin_status+Total_chol,0.5 ,data = dat)



library(ggplot2)
mydf <- ggpredict(fit, terms ="Age[all]")
plot(mydf)

#####
mydf=cbind(mydf,dat)
ggplot(mydf, aes(x, y=predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
    #geom_quantile(formula = y ~bs(Age,df=4) , quantiles = c(0.2))+
    #geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    #ylim(c(20,50))+
    theme_bw(base_size = 8)#+
#facet_grid(Race~Gender)

#############################
dat=readRDS(file = "bmi.data.rds")
#dat=dat[dat$BMI<28,]
dat=filter(dat,Statin_status!='9')
levels(dat$Statin_status) <- c('1','2')
dat$Statin_status <- factor(dat$Statin_status)
X <- model.matrix( BMI ~ bs(Total_chol, df = 5) + Race + Gender + Statin_status, data = dat)
qr_fit_dat <- rq(  dat$BMI ~ X - 1, data=dat, tau = 0.9)


#x<-1:50
#y<-c(x[1:48]+rnorm(48,0,5),rnorm(2,150,5))

#QR <- rq(formula,bmi.data, tau=0.5)
QR=qr_fit_dat
summary(QR, se='boot')

LM<-lm(y~x)

QR.b <- boot.rq(cbind(1,X),dat$BMI,tau=0.9, R=1000)

t(apply(QR.b$B, 2, quantile, c(0.025,0.975)))
confint(LM)


plot(dat$Total_chol,dat$BMI,xlim = c(100,400),ylim=c(20,60))
abline(coefficients(LM),col="green")
abline(coefficients(QR),col="blue")

for(i in seq_len(nrow(QR.b$B))) {
    abline(QR.b$B[i,1], QR.b$B[i,2], col='#0000ff01')
}
 
