# Plot of quantile regression for glucose reressed on Gender, Age,Race,BMI,Statin_status,BMI
require(quantreg)
library(tidyverse)
library(splines)

bmi.data<- read.csv(here::here("data", "ldl2.csv"),header=TRUE, sep=",")

#remove duplicated rows and remove na data.
bmi.data=bmi.data[!duplicated(bmi.data[ , c("SEQN")]),]

bmi.data=data.frame(bmi.data$RIAGENDR,bmi.data$RIDAGEYR, bmi.data$RIDRETH1,bmi.data$BMXBMI,
                    bmi.data$LBXTC,bmi.data$TCRx,bmi.data$LBXGLU,bmi.data$BMXWAIST)
#bmi.data= na.omit(bmi.data[,c(5,6,9,29,33,43,32)])
#bmi.data=bmi.data[bmi.data$bmi.data.diab_drug==2,]
#bmi.data=na.omit(bmi.data[,c(1:7)])
colnames(bmi.data) <- c("Gender","Age","Race","BMI","Total_chol" ,"Statin_status","Glu","waist_cir")
bmi.data=na.omit(bmi.data)
bmi.data=bmi.data%>%filter(bmi.data$Age>19)
bmi.data$Gender=recode(bmi.data$Gender, "1" = "Male", "2" = "Female" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

bmi.data$Race=recode(bmi.data$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
# Classifying population to diabetes and non diabetes
bmi.data=bmi.data[1:1000,]

#formula <- Glu ~Gender+Age+Race+BMI+Statin_status+Age2+BMI2+Total_chol+Total_col2+Waist_cir 
formula <- Glu ~Gender+Race+BMI+Statin_status#+bs(Age,intercept=FALSE,df=5)+bs(Total_chol,intercept=FALSE,df=5)+bs(waist_cir,intercept=FALSE,df=5)      
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

# Quantile
 
################
l=rq(data=bmi.data, 
     tau= 1:9/10,
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

