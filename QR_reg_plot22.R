# Plot of quantile regression for BMI regressed on Gender, Age,Race, and Cholestrol medication use.
#Using ggplot
require(quantreg)
library(tidyverse)
library(splines)
 
bmi.data=readRDS(file="bmi.data.rds")
#bmi.data=bmi.data[c(1:1000),]
 #formula <- Glu ~Gender+Age+Race+BMI+Statin_status+Age2+BMI2+Total_chol+Total_col2+Waist_cir 
formula <- BMI ~Gender+Race+Statin_status+bs(Age,intercept=FALSE,df=5)+bs(Total_chol,intercept=FALSE,df=5)       
fit3.ols <- summary(lm(formula,data =bmi.data))$coefficients
#attach(bmi.data)
p <- nrow(fit3.ols)
taus <- c(1:4/100, 1:19/20)
fit3 <- array(fit3.ols,c(p,4,length(taus)))
rownames(fit3)=rownames(fit3.ols)
colnames(fit3)=colnames(fit3.ols)
dimnames(fit3)[[3]]=taus
fit3["Age2","Estimate",'0.05']
 

#######################
 

# OLS
lm <- lm(data=bmi.data,
         formula =formula)

ols <- as.data.frame(coef(lm))
ols.ci <- as.data.frame(confint(lm))
ols2 <- cbind(ols, ols.ci)
ols2 <- tibble::rownames_to_column(ols2, var="term")
#ols2 <-ols2%>%filter(term %in% c('RaceMexican','Statin_status'))

# Quantile
 
################
l=rq(data=bmi.data, 
     tau= 1:9/10,
     formula =BMI ~Gender+Race+Statin_status 
         +bs(Age,intercept=FALSE,df=5)+bs(Total_chol,intercept=FALSE,df=5))%>%

    broom::tidy(se.type = "boot")%>%
   filter(!grepl("factor", term))
l$conf.low=l$estimate+ 2*l$std.error
l$conf.high=l$estimate- 2*l$std.error

png(file = here::here("images", "newfig111.png"),
    res = 400, height = 9, width = 16, units = "in")

l%>%filter(term %in% c('RaceMexican','Statin_status'))%>%
    ggplot(aes(x=tau,y=estimate))+
    
    # quantilie results
    geom_point(color="#27408b", size = 3)+ 
    geom_line(color="#27408b", size = 1)+ 
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.25, fill="#27408b")+
     # OLS results
    geom_hline(data = ols2, aes(yintercept= `coef(lm)`), lty=1, color="red", size=1)+
    geom_hline(data = ols2, aes(yintercept= `2.5 %`), lty=2, color="red", size=1)+
    geom_hline(data = ols2, aes(yintercept= `97.5 %`), lty=2, color="red", size=1)+
    facet_wrap(~term, scales="free", ncol=2)    

dev.off()




#####################################


l=rq(data=bmi.data, 
     tau= 0.25,
     formula =Glu ~Gender+Race+BMI+Statin_status )%>%#+bs(Age,intercept=FALSE,df=5)+
    # bs(Total_chol,intercept=FALSE,df=5)+bs(waist_cir,intercept=FALSE,df=5))
    
    broom::tidy(se.type = "boot")%>%
    filter(!grepl("factor", term))
l$conf.low=l$estimate+ 2*l$std.error
l$conf.high=l$estimate- 2*l$std.error

ggplot(l,aes(x=tau,y=estimate))+
    
    # quantilie results
    geom_point(color="#27408b", size = 3)+ 
    geom_line(color="#27408b", size = 1)+ 
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.25, fill="#27408b")+
    
    # OLS results
    geom_hline(data = ols2, aes(yintercept= `coef(lm)`), lty=1, color="red", size=1)+
    geom_hline(data = ols2, aes(yintercept= `2.5 %`), lty=2, color="red", size=1)+
    geom_hline(data = ols2, aes(yintercept= `97.5 %`), lty=2, color="red", size=1)+
    facet_wrap(~term, scales="free", ncol=2)    


#####################################

####################################################################################3
 
 





