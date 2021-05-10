
#Marginal effect on BMI with splines
#Plot for the confidence interval of BMI regressed on splines of Age, 
#race, gender,and cholesterol medications use.


library(quantreg)
library(splines)
library(tidyverse)
library(ggeffects)
library(gridExtra)




dat=readRDS(file = "bmi.data.rds")
#dat=dat[dat$BMI<28,]
dat=filter(dat,Cholesterol_Drug_Use!='9')
#levels(dat$Cholesterol_Drug_Use) <- c('1','2')
dat$Cholesterol_Drug_Use <- factor(dat$Cholesterol_Drug_Use)
dat$Age=as.numeric(dat$Age)

#formula 1
#formula=BMI ~ bs(Age, df = 5) + Race + Gender + Cholesterol_Drug_Use+Total_chol
#formula 2
#formula=BMI ~ bs(Total_chol, df=5)+ Race + Gender + Cholesterol_Drug_Use
# formula 3 
#formula=BMI ~ Age+Age^2+Total_chol+ Total_chol^2+ Race + Gender + Cholesterol_Drug_Use


fit_rq=function(dat,index,tau){
    if (index==1) {formula=BMI ~ bs(Age, df = 5) + Race + Gender + Cholesterol_Drug_Use}
    else if(index ==2) {formula=BMI ~ bs(Total_chol, df=5)+ Race + Gender + Cholesterol_Drug_Use}
    else if (index==3) {formula=BMI ~ Age+Age^2+ Race + Gender + Cholesterol_Drug_Use}
    else{formula=BMI ~ Total_chol+ Total_chol^2+ Race + Gender + Cholesterol_Drug_Use}
    message("model.matrix")
    X <- model.matrix(formula,data=dat)
    # qr_fit_dat <- rq( BMI ~ X - 1, data=dat, tau = tau)
    qr_fit_dat <- rq( formula, data=dat, tau = tau)
    return(qr_fit_dat)
}

#index= 1 bsplines age and total cholesterol, Index=2 bspline total choles, 
#and index=3 polynomial age and total choles.
fit=fit_rq(dat,index=1,tau=0.9)


#ggeffect plot

ggpredict(fit,"Age [all]")
df=ggpredict(fit,"Age [all]")
ggplot(df,aes(x,predicted))+
    geom_line()+
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.1)


#Total_chol
fit=fit_rq(dat,index=2,tau=0.9)


ggpredict(fit,"Total_chol [all]")
df=ggpredict(fit,"Total_chol [all]")
ggplot(df,aes(x,predicted))+
    geom_line()+
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.1)

#Age
fit=fit_rq(dat,index=3,tau=0.9)
ggpredict(fit,"Age")
df=ggpredict(fit,"Age")
ggplot(df,aes(x,predicted))+
    geom_line()+
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.1)

#
fit=fit_rq(dat,index=4,tau=0.9)
ggpredict(fit,"Total_chol")
df=ggpredict(fit,"Total_chol")
ggplot(df,aes(x,predicted))+
    geom_line()+
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.1)



#####

 
fit=fit_rq(dat,index=1,tau=0.9)
fit1=ggpredict(fit,"Age [all]") 
fit=fit_rq(dat,index=2,tau=0.9)
fit2=ggpredict(fit,"Total_chol [all]")
fit=fit_rq(dat,index=3,tau=0.9)
fit3=ggpredict(fit,"Age")
fit=fit_rq(dat,index=4,tau=0.9)
fit4=ggpredict(fit,"Total_chol")

dat1 <- rbind(
    fit1 %>%
        mutate(model = "Spline"),
    fit3 %>%
        mutate(model = "Quadratic"))


 tot1= ggplot(dat1, aes(x = x, y = predicted, group = model, color = model, fill = model)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
    xlab("Age")+
    ylab("BMI")
 png(file = here::here("images", "comp_age.png"),
     res = 400, height = 9, width = 16, units = "in")
  print(tot1) 
 dev.off()

##############
#10%
 
 fit=fit_rq(dat,index=1,tau=0.1)
 fit1=ggpredict(fit,"Age [all]") 
 fit=fit_rq(dat,index=3,tau=0.1)
 fit3=ggpredict(fit,"Age")
 
 
 dat1 <- rbind(
   fit1 %>%
     mutate(model = "Spline"),
   fit3 %>%
     mutate(model = "Quadratic"))
 
 
 tot01= ggplot(dat1, aes(x = x, y = predicted, group = model, color = model, fill = model)) +
   geom_line() +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
   xlab("Age")+
   ylab("BMI")+
   ggtitle("Age effect on BMI 0.01 Quantile ")
 
#####################
# 0.5% 
 fit=fit_rq(dat,index=1,tau=0.5)
 fit1=ggpredict(fit,"Age [all]") 
 fit=fit_rq(dat,index=3,tau=0.5)
 fit3=ggpredict(fit,"Age")
 
 
 dat1 <- rbind(
   fit1 %>%
     mutate(model = "Spline"),
   fit3 %>%
     mutate(model = "Quadratic"))
 
 
 tot05= ggplot(dat1, aes(x = x, y = predicted, group = model, color = model, fill = model)) +
   geom_line() +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
   xlab("Age")+
   ylab("BMI")+
   ggtitle("Age effect on BMI 0.05 Quantile ")
 
 
 ##########################
 #0.75
 
 fit=fit_rq(dat,index=1,tau=0.75)
 fit1=ggpredict(fit,"Age [all]") 
 fit=fit_rq(dat,index=3,tau=0.75)
 fit3=ggpredict(fit,"Age")
 
 
 dat1 <- rbind(
   fit1 %>%
     mutate(model = "Spline"),
   fit3 %>%
     mutate(model = "Quadratic"))
 
 
 tot075= ggplot(dat1, aes(x = x, y = predicted, group = model, color = model, fill = model)) +
   geom_line() +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
   xlab("Age")+
   ylab("BMI")+
   ggtitle("Age effect on BMI 0.75 Quantile ")
 
 
 #############
 # 0.9
 
 fit=fit_rq(dat,index=1,tau=0.9)
 fit1=ggpredict(fit,"Age [all]") 
 fit=fit_rq(dat,index=3,tau=0.9)
 fit3=ggpredict(fit,"Age")
 
 
 dat1 <- rbind(
   fit1 %>%
     mutate(model = "Spline"),
   fit3 %>%
     mutate(model = "Quadratic"))
 
 
 tot09= ggplot(dat1, aes(x = x, y = predicted, group = model, color = model, fill = model)) +
   geom_line() +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
   xlab("Age")+
   ylab("BMI")+
   ggtitle("Age effect on BMI 0.09 Quantile ")
 
 
 ############
 #Plot 
 
 png(file = here::here("images", "comp_age_mixed.png"),
     res = 400, height = 9, width = 16, units = "in")
 # print(rr)
 grid.arrange(tot01, tot05,tot075,tot09, nrow=2, ncol=2)
 dev.off()
 
 
 ###############################



rr=grid.arrange(tot1, tot01, nrow=1, ncol=2)

png(file = here::here("images", "comp_5.png"),
    res = 400, height = 9, width = 16, units = "in")
# print(rr)
grid.arrange(tot1, tot01, nrow=1, ncol=2)
dev.off()




################
#
##### Grid for Total cholesterol
#


fit=fit_rq(dat,index=2,tau=0.1)
fit2=ggpredict(fit,"Total_chol [all]")
fit=fit_rq(dat,index=4,tau=0.1)
fit4=ggpredict(fit,"Total_chol")


dat2 <- rbind(
  fit2 %>%
    mutate(model = "Spline"),
  fit4 %>%
    mutate(model = "Quadratic"))


tot_chol01= ggplot(dat2, aes(x = x, y = predicted, group = model, color = model, fill = model)) +
  geom_line() +
  ylim(c(15,50))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  xlab("Total Cholesterol")+
  ylab("BMI")+
  ggtitle("Total cholesterol effect on BMI 0.1 Quantile ")

################
####

fit=fit_rq(dat,index=2,tau=0.5)
fit2=ggpredict(fit,"Total_chol [all]")
fit=fit_rq(dat,index=4,tau=0.5)
fit4=ggpredict(fit,"Total_chol")


dat2 <- rbind(
  fit2 %>%
    mutate(model = "Spline"),
  fit4 %>%
    mutate(model = "Quadratic"))


tot_chol05= ggplot(dat2, aes(x = x, y = predicted, group = model, color = model, fill = model)) +
  geom_line() +
  ylim(c(20,50))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  xlab("Total Cholesterol")+
  ylab("BMI")+
  ggtitle("Total cholesterol effect on BMI 0.5 Quantile ")

############
#0.75

fit=fit_rq(dat,index=2,tau=0.75)
fit2=ggpredict(fit,"Total_chol [all]")
fit=fit_rq(dat,index=4,tau=0.75)
fit4=ggpredict(fit,"Total_chol")


dat2 <- rbind(
  fit2 %>%
    mutate(model = "Spline"),
  fit4 %>%
    mutate(model = "Quadratic"))


tot_chol075= ggplot(dat2, aes(x = x, y = predicted, group = model, color = model, fill = model)) +
  geom_line() +
  ylim(c(20,50))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  xlab("Total Cholesterol")+
  ylab("BMI")+
  ggtitle("0.75 Quantile ")

 ####################
##0.9

fit=fit_rq(dat,index=2,tau=0.9)
fit2=ggpredict(fit,"Total_chol [all]")
fit=fit_rq(dat,index=4,tau=0.9)
fit4=ggpredict(fit,"Total_chol")


dat2 <- rbind(
  fit2 %>%
    mutate(model = "Spline"),
  fit4 %>%
    mutate(model = "Quadratic"))


tot_chol09= ggplot(dat2, aes(x = x, y = predicted, group = model, color = model, fill = model)) +
  geom_line() +
  ylim(c(20,50))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  xlab("Total Cholesterol")+
  
  ylab("BMI")

png(file = here::here("images", "comp_tot_mixed.png"),
    res = 400, height = 9, width = 16, units = "in")
# print(rr)
grid.arrange( tot_chol01,tot_chol05,tot_chol075,tot_chol09, nrow=2, ncol=2)
dev.off()

