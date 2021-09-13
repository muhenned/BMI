
#Marginal effect on BMI with splines
#Plot for the confidence interval of BMI regressed on splines of Age, 
#race, gender,and cholesterol medications use.


library(quantreg)
library(splines)
library(tidyverse)
library(ggeffects)
library(gridExtra)






############################################
bmi.data<- read.csv(here::here("data", "ldl3.csv"),header=TRUE, sep=",")

#remove duplicated rows and remove na data.
bmi.data=bmi.data[!duplicated(bmi.data[ , c("SEQN")]),]


bmi.data=bmi.data[bmi.data$TCRx !=2,]
bmi.data=bmi.data[bmi.data$RIDAGEYR>19,] 

bmi.data=data.frame(bmi.data$RIAGENDR,bmi.data$RIDAGEYR, bmi.data$RIDRETH1,bmi.data$BMXBMI,
                    bmi.data$LBXTC,bmi.data$TCRx,bmi.data$LBXGLU,bmi.data$BMXWAIST)
#bmi.data= na.omit(bmi.data[,c(5,6,9,29,33,43,32)])
#bmi.data=bmi.data[bmi.data$bmi.data.diab_drug==2,]
#bmi.data=na.omit(bmi.data[,c(1:7)])
colnames(bmi.data) <- c("Gender","Age","Race","BMI","Total_chol" ,"Statin_status","Glu","Waist_cir")
bmi.data=na.omit(bmi.data)
bmi.data=bmi.data%>%filter(bmi.data$Age>19)
#bmi.data$Gender=recode(bmi.data$Gender, "1" = "Male", "2" = "Female" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

#bmi.data$Race=recode(bmi.data$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
# Classifying population to diabetes and non diabetes

bmi.data <- cbind(bmi.data, Age2 = bmi.data$Age^2)
bmi.data <- cbind(bmi.data, BMI2 = bmi.data$BMI^2)
bmi.data <- cbind(bmi.data, Total_col2 = bmi.data$Total_chol^2)
formula <- Glu ~Gender+Age+Race+BMI+Statin_status+Age2+BMI2+Total_chol+Total_col2+Waist_cir 
#formula <- Glu ~Gender+Race+BMI+Statin_status+Age+Total_chol+Waist_cir      
fit3.ols <- summary(lm(formula,data =bmi.data))$coefficients
#attach(bmi.data)
p <- nrow(fit3.ols)
taus <- c(1:4/100, 1:19/20)
fit3 <- array(fit3.ols,c(p,4,length(taus)))
rownames(fit3)=rownames(fit3.ols)
colnames(fit3)=colnames(fit3.ols)
dimnames(fit3)[[3]]=taus
fit3["Age2","Estimate",'0.05']
# for(i in 1:length(taus)){
#   print(taus[i])
#   f <- rq(formula, taus[i], data = bmi.data, method="fn")
#   fit3[,,i] <- summary(f)$coefficients
# }

############################################################################

dat=bmi.data

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
    if (index==1) {formula <- Glu ~Gender+Age+Race+BMI+Statin_status+Age2+BMI2+Total_chol+Total_col2+Waist_cir}
    else if(index ==2) {formula=Glu ~ bs(Total_chol, df=5)+ Race + Gender + Statin_status}
    else if (index==3) {formula=Glu ~poly(Age,2)+ Race + Gender + Statin_status}
    else{formula=Glu ~ poly(Total_chol,2)+ Race + Gender + Statin_status}
    message("model.matrix")
    X <- model.matrix(formula,data=dat)
    qr_fit_dat <- rq( Glu ~ X - 1, data=dat, tau = tau)
    #qr_fit_dat <- rq( formula, data=dat, tau = tau)
    return(qr_fit_dat)
}

#index= 1 (The one that I am interested in)  
fit=fit_rq(dat,index=1,tau=0.9)





#plot
dat_pred_ready=function(dat,index,tau){
    age_pred <- seq(16, 80, by = 2)
    x2_pred <- factor(1:5)
    x3_pred <- factor(1:2)
    x4_pred <- factor(0:1)
    x5_pred <-seq(min(dat$Total_chol),400,by=10)
    x6_pred <-seq(min(dat$BMI),60,by=1)
    x7_pred <-seq(min(dat$Waist_cir),170,by=7)
    if(index==1){
        dat_pred <- data.frame(expand.grid(age_pred, x2_pred, x3_pred, x4_pred,x5_pred,x6_pred,x7_pred))
        names(dat_pred) <- c( "Age","Race", "Gender", "Statin_status","Total_chol","BMI", "Waist_cir")
    }else if(index==2){
        dat_pred <- data.frame(expand.grid(x5_pred, x2_pred, x3_pred, x4_pred))
        names(dat_pred) <- c("Total_chol", "Race", "Gender", "Statin_status" )
    }else if(index==3) { message(" pred")
        dat_pred <- data.frame(expand.grid(age_pred, x2_pred, x3_pred, x4_pred,x5_pred))
        names(dat_pred) <- c( "Age","Race", "Gender", "Statin_status","Total_chol" )
    } else{
        dat_pred <- data.frame(expand.grid(age_pred, x2_pred, x3_pred, x4_pred,x5_pred))
        names(dat_pred) <- c( "Age","Race", "Gender", "Statin_status","Total_chol" )
    }
    
    
    dat_pred$Gender=recode(dat_pred$Gender, "1" = "Male", "2" = "Female" )
    
    
    dat_pred$Race=recode(dat_pred$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
    
    if(index==1){
        X_pred <- model.matrix(~ Gender+Age+Race+BMI+poly(BMI,2)+Statin_status+poly(Age,2)+Total_chol+poly(Total_chol,2)+Waist_cir, data = dat_pred)
        
    }else if(index==2){
        X_pred <- model.matrix(~ bs(Total_chol, df=5)+ Race + Gender + Statin_status,data=dat_pred)
    }else if (index==3) { message("pred.model.matrix")
        X_pred <- model.matrix(~poly(Age,2)+ Race + Gender + Statin_status,data=dat_pred)
    } else{X_pred <- model.matrix(~poly(Total_chol,2)+ Race + Gender + Statin_status,data=dat_pred)
    
    }
    
    
    fit=fit_rq(dat,index,tau)
    
    dat_prediction <- list(X = X_pred)
    message("A")
    pred <- predict(fit, newdata =dat_prediction, interval = "confidence")
    message("B")
    dat_pred$pred  <- pred[, 1]
    dat_pred$lower  <- pred[, 2]
    dat_pred$higher <- pred[, 3]
    return(dat_pred)
}

dat_pred=dat_pred_ready(dat,index =1 ,tau =0.9 )
# Get model-based predictions

dat_pred$Statin_status=recode(dat_pred$Statin_status, "0" = "No", "1" = "Yes" )

###############################################################################4

fig2=dat_pred %>%
    ggplot(aes(x = Age, y = pred, color = Statin_status)) +
    geom_line() +
    #geom_quantile(formula = y ~ bs(x,intercept=FALSE,df=5), quantiles = 0.25)+
    geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    #ylim(c(20,70))+
    theme_bw(base_size = 15)+
    facet_grid(Race~Gender)

# png(file = here::here("images", "Age_splin.png"),
#     res = 400, height = 9, width = 16, units = "in")
# print(fig2)
# dev.off()


###################
dat_pred=dat_pred_ready(dat,index =2 ,tau =0.9 )

dat_pred$Statin_status=recode(dat_pred$Statin_status, "0" = "No", "1" = "Yes" )

# plot for splines on total cholestrol
fig3=dat_pred %>%filter(Total_chol<400)%>%
    ggplot(aes(x = Total_chol, y = pred, color = Statin_status)) +
    geom_line() +
    #geom_quantile(formula = y ~ bs(x,intercept=FALSE,df=5), quantiles = 0.25)+
    geom_ribbon(aes(x =Total_chol  , ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    #ylim(c(30,70))+
    xlim(c(100,375))+
    theme_bw(base_size = 15)+
    #geom_point(data=dat, aes(x=Total_chol,y=Glu),inherit.aes = FALSE,alpha=0.05)+
    facet_grid(Race~Gender)


# png(file = here::here("images", "TC_splin.png"),
#     res = 400, height = 9, width = 16, units = "in")
# print(fig3) 
# dev.off()


########################

dat_pred=dat_pred_ready(dat,index =3 ,tau =0.9 )

dat_pred$Statin_status=recode(dat_pred$Statin_status, "0" = "No", "1" = "Yes" )

# plot for splines on total cholestrol
fig3=dat_pred %>% 
    ggplot(aes(x = Age, y = pred, color = Statin_status)) +
    geom_line() +
    #geom_quantile(formula = y ~ bs(x,intercept=FALSE,df=5), quantiles = 0.25)+
    geom_ribbon(aes(x =Age  , ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    #ylim(c(30,70))+
    theme_bw(base_size = 15)+
    #geom_point(data=dat, aes(x=Total_chol,y=Glu),inherit.aes = FALSE,alpha=0.05)+
    facet_grid(Race~Gender)


png(file = here::here("images", "Age_poly_race01.png"),
    res = 400, height =9, width = 16, units = "in")
print(fig3) 
dev.off()


###########################

dat_pred=dat_pred_ready(dat,index =4 ,tau =0.9 )

dat_pred$Statin_status=recode(dat_pred$Statin_status, "0" = "No", "1" = "Yes" )

# plot for splines on total cholesterol
fig4=dat_pred %>%filter(Total_chol<400)%>%
    ggplot(aes(x = Total_chol, y = pred, color = Statin_status)) +
    geom_line() +
    #geom_quantile(formula = y ~ bs(x,intercept=FALSE,df=5), quantiles = 0.25)+
    geom_ribbon(aes(x =Total_chol  , ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
    scale_color_viridis_d(end = 0.7) +
    scale_fill_viridis_d(end = 0.7)+
    #ylim(c(30,70))+
    xlim(c(100,375))+
    theme_bw(base_size = 15)+
    #geom_point(data=dat, aes(x=Total_chol,y=Glu),inherit.aes = FALSE,alpha=0.05)+
    facet_grid(Race~Gender)


# png(file = here::here("images", "Total_chol_poly.png"),
#     res = 400, height = 9, width = 16, units = "in")
# print(fig4) 
# dev.off()






# 
# ################################################################################################
# 
# fit <- rq(Glu ~ bs(Age,df=4)+Race+Gender+Statin_status+bs(Total_chol,df=4),0.9 ,data = dat)
# #fit <- rq(BMI ~ Age+Race+Gender+Statin_status+Total_chol,0.5 ,data = dat)
# 
# 
# 
# library(ggplot2)
# mydf <- ggpredict(fit, terms ="Total_chol[all]")
# mydf=cbind(mydf,dat)
# ggplot(mydf, aes(x, y=predicted)) +
#     geom_line() +
#     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
#     #geom_quantile(formula = y ~bs(Total_chol,df=4) , quantiles = c(0.2))+
#     #geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
#     scale_color_viridis_d(end = 0.7) +
#     scale_fill_viridis_d(end = 0.7)+
#     #ylim(c(20,50))+
#     theme_bw(base_size = 8)#+
# #facet_grid(Race~Gender)
# 
# #######################################################
# 
# 
# 
# fit <- rq(BMI ~ bs(Age,df=4)+Race+Gender+Statin_status+Total_chol,0.1 ,data = dat)
# #fit <- rq(BMI ~ Age+Race+Gender+Statin_status+Total_chol,0.5 ,data = dat)
# 
# 
# 
# library(ggplot2)
# mydf <- ggpredict(fit, terms ="Age[all]")
# plot(mydf)
# 
# #####
# mydf=cbind(mydf,dat)
# ggplot(mydf, aes(x, y=predicted)) +
#     geom_line() +
#     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
#     #geom_quantile(formula = y ~bs(Age,df=4) , quantiles = c(0.2))+
#     #geom_ribbon(aes(x = Age, ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
#     scale_color_viridis_d(end = 0.7) +
#     scale_fill_viridis_d(end = 0.7)+
#     #ylim(c(20,50))+
#     theme_bw(base_size = 8)#+
# #facet_grid(Race~Gender)
# 
# #############################
# dat=readRDS(file = "bmi.data.rds")
# #dat=dat[dat$BMI<28,]
# dat=filter(dat,Statin_status!='9')
# levels(dat$Statin_status) <- c('1','2')
# dat$Statin_status <- factor(dat$Statin_status)
# X <- model.matrix( BMI ~ bs(Total_chol, df = 5) + Race + Gender + Statin_status, data = dat)
# qr_fit_dat <- rq(  dat$BMI ~ X - 1, data=dat, tau = 0.9)
# 
# 
# #x<-1:50
# #y<-c(x[1:48]+rnorm(48,0,5),rnorm(2,150,5))
# 
# #QR <- rq(formula,bmi.data, tau=0.5)
# QR=qr_fit_dat
# summary(QR, se='boot')
# 
# LM<-lm(y~x)
# 
# QR.b <- boot.rq(cbind(1,X),dat$BMI,tau=0.9, R=1000)
# 
# t(apply(QR.b$B, 2, quantile, c(0.025,0.975)))
# confint(LM)
# 
# 
# plot(dat$Total_chol,dat$BMI,xlim = c(100,400),ylim=c(20,60))
# abline(coefficients(LM),col="green")
# abline(coefficients(QR),col="blue")
# 
# for(i in seq_len(nrow(QR.b$B))) {
#     abline(QR.b$B[i,1], QR.b$B[i,2], col='#0000ff01')
# }
# 
# ###########################
# 
# 
# tau=c(0.1,0.25,0.55,0.75,0.90)
# for(i in 1:5){
#     dat_pred=dat_pred_ready(dat,index =2 ,tau =tau[i] ) 
#     # plot for splines on total cholestrol
#     fig[i]=dat_pred %>%filter(Total_chol<375)%>%
#         ggplot(aes(x = Total_chol, y = pred, color = Statin_status)) +
#         geom_line() +
#         #geom_quantile(formula = y ~ bs(x,intercept=FALSE,df=5), quantiles = 0.25)+
#         geom_ribbon(aes(x =Total_chol  , ymin = lower, ymax = higher, fill = Statin_status), alpha = 0.1) +
#         scale_color_viridis_d(end = 0.7) +
#         scale_fill_viridis_d(end = 0.7)+
#         #ylim(c(30,70))+
#         xlim(c(100,375))+
#         theme_bw(base_size = 8)+
#         #geom_point(data=dat, aes(x=Total_chol,y=BMI),inherit.aes = FALSE,alpha=0.05)+
#         facet_grid(Race~Gender)
#     print(fig[i])  
# }
# #png(file = here::here("images", "Age_splin.png"),
# res = 400, height = 9, width = 16, units = "in")
# grid.arrange(fig[1],fig[2],fig[3],fig[4],fig[5], ncol=3, nrow = 2)
# dev.off()
# 


################
#ggeffect plot

ggpredict(fit,"Age [all]")
df=ggpredict(fit,"Age [all]")
ggplot(df,aes(x,predicted))+
    geom_line()+
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.1)

