# Plot of quantile regression for glucose reressed on Gender, Age,Race,BMI,Statin_status,BMI
require(quantreg)
library(tidyverse)
bmi.data<- read.csv(here::here("data", "ldl2.csv"),header=TRUE, sep=",")

#remove duplicated rows and remove na data.
bmi.data=bmi.data[!duplicated(bmi.data[ , c("SEQN")]),]
#both DID070 and DIQ 070 represent an answer for question Are you useing medication for glucose
#yes 1 no 2
bmi.data$diab_drug=coalesce(bmi.data$DIQ070,bmi.data$DID070)
 
bmi.data=data.frame(bmi.data$RIAGENDR,bmi.data$RIDAGEYR, bmi.data$RIDRETH1,bmi.data$BMXBMI,
                    bmi.data$LBXTC,bmi.data$TCRx,bmi.data$LBXGLU,bmi.data$BMXWAIST)
#bmi.data= na.omit(bmi.data[,c(5,6,9,29,33,43,32)])
#bmi.data=bmi.data[bmi.data$bmi.data.diab_drug==2,]
#bmi.data=na.omit(bmi.data[,c(1:7)])
colnames(bmi.data) <- c("Gender","Age","Race","BMI","Total_chol" ,"Statin_status","Glu","Waist_cir")
bmi.data=na.omit(bmi.data)
bmi.data=bmi.data%>%filter(bmi.data$Age>19)
bmi.data$Gender=recode(bmi.data$Gender, "1" = "Male", "2" = "Female" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

bmi.data$Race=recode(bmi.data$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
# Classifying population to diabetes and non diabetes

bmi.data <- cbind(bmi.data, Age2 = bmi.data$Age^2)
bmi.data <- cbind(bmi.data, BMI2 = bmi.data$BMI^2)
bmi.data <- cbind(bmi.data, Total_col2 = bmi.data$Total_chol^2)
formula <- Glu ~Gender+Age+Race+BMI+Statin_status+Age2+BMI2+Total_chol+Total_col2+Waist_cir 
     
fit3.ols <- summary(lm(formula,data =bmi.data))$coefficients
#attach(bmi.data)
p <- nrow(fit3.ols)
taus <- c(1:4/100, 1:19/20)
fit3 <- array(fit3.ols,c(p,4,length(taus)))
for(i in 1:length(taus)){
    print(taus[i])
    f <- rq(formula, taus[i], data = bmi.data, method="fn")
    fit3[,,i] <- summary(f)$coefficients
}
##
Value   Std. Error     t value     Pr(>|t|)
(Intercept)         66.876965374 22.109483682  3.02480901 0.0024927857
GenderMale           9.388121062  2.726566710  3.44320241 0.0005766896
Age                  1.063354442  0.390725987  2.72148379 0.0065076556
RaceMexican          5.873750677  7.489126545  0.78430384 0.4328762137
RaceOther           -7.667518830  5.547297354 -1.38220801 0.1669317118
RaceOther_Hispanic   3.958837918  8.889960955  0.44531556 0.6560991799
RaceWhite          -13.641734789  5.143504805 -2.65222554 0.0080060692
BMI                 -0.103898456  1.565017907 -0.06638803 0.9470699260
Statin_status       52.415760906  6.008551226  8.72352734 0.0000000000
Age2                -0.001569404  0.004050069 -0.38750057 0.6983920391
BMI2                 0.035566924  0.026797265  1.32725949 0.1844462891
Total_Chol           0.033962593  0.041216181  0.82401115 0.4099484171
##
##########################################################
########################################################################
#########################################################


#Plot results from newfit.R:
#Formula:
# Use these colors for PostScript on the next device startup.
cols <- c("black","red", "blue","LightSkyBlue1", "green", "light pink")
#pdf("newfig.pdf",width=8.5,height=7)
#pdf("./images/newfig.pdf",width=15,height=13)
png(file = here::here("images", "newfig.png"),
    res = 400, height = 9, width = 16, units = "in")
p <- dim(fit3)[1]
blab <- c("Intercept","Male ","Age","RaceMexican", "RaceOther", "RaceOther_Hispanic ", "White ", "BMI", " Statin_status","Age Suqared","BMI^2","Total cholesterol","Total cholestrol^2","Waist Circumference")
 
par(mfrow=c(4,4))
for(i in c(1:14)){
    if(i==1){#adjust intercept to be centercept
        Age.bar <- mean(bmi.data$Age)
        BMI.bar <- mean(bmi.data$BMI)
        b <- fit3[i,1,]+
            Age.bar*fit3[3,1,]+(Age.bar^2)*fit3[10,1,]+
            BMI.bar*fit3[8,1,]+(BMI.bar^2)*fit3[11,1,]
    }
    else{
        b <- fit3[i,1,]
    }
    
    b.p <- b + qnorm(.95)*fit3[i,2,]
    b.m <- b - qnorm(.95)*fit3[i,2,]
    plot(0:1,range(c(b.m,b.p)),type="n",xlab="",ylab="",cex=.75)
    title(paste(blab[i]),cex=.75)
    polygon(c(taus,rev(taus)),c(b.p,rev(b.m)), col=cols[4])
    points(taus,b, col=cols[3])
    lines(taus,b,col=cols[3])
    abline(h=0)
    if(i==1){#now fix ols results
    
    bhat <- fit3.ols[i,1]+
        Age.bar*fit3.ols[3,1]+(Age.bar^2)*fit3.ols[10,1]+
        BMI.bar*fit3.ols[8,1]+(BMI.bar^2)*fit3.ols[11,1]
    }
    else{
        bhat <- fit3.ols[i,1]
    }
    bhat.se <- fit3.ols[i,2]
    abline(h=bhat,col=cols[2])
    abline(h=bhat-qnorm(.95)*bhat.se,col=cols[6])
    abline(h=bhat+qnorm(.95)*bhat.se,col=cols[6])
}
dev.off()
#plot for marginal effect of weight gain for several gains
#pdf("newfig2.pdf",width=7.0,height=7.0)
png(file = here::here("images", "newfig2.png"),
    res = 400, height = 9, width = 16, units = "in")
par(mfrow=c(2,2))
gains <- quantile(bmi.data$BMI,c(.1,.25,.75,.9))
for(i in 1:length(gains)){
    effect <- fit3[8,1,]+2*fit3[11,1,]*gains[i]
    plot(taus,effect,xlab="Quantile",ylab="Weight Gain Effect")
    lines(taus,effect)
    title(paste("Weight Gain", format(round(gains[i])),"Lbs"))
}
dev.off()
#Now try to plot quadratic effect of mother's age for several taus
#pdf("newfig3.pdf",width=7.0,height=7.0)
png(file = here::here("images", "newfig3.png"),
    res = 400, height = 9, width = 16, units = "in")

par(mfrow=c(2,2))
ages <- seq(min(bmi.data$Age),max(bmi.data$Age),by=1)
for(i in c(6,9,15,22)){
    effect <- fit3[3,1,i]*ages+fit3[10,1,i]*ages^2
    plot(ages,effect,type="n",xlab="Age",ylab="Age effect")
    lines(ages,effect)
    title(paste("Age effects on glucose", format(round(taus[i],2)),"Quantile"))
}
dev.off()

#Total cholestrol effects on glucose.

png(file = here::here("images", "newfig4.png"),
    res = 400, height = 9, width = 16, units = "in")

par(mfrow=c(2,2))
Total_chol <- seq(min(bmi.data$Total_chol),300,by=1)
for(i in c(6,9,15,22)){
    effect <- fit3[12,1,i]*Total_chol +fit3[13,1,i]*Total_chol^2
    plot(Total_chol,effect,type="n",xlab="Total cholestrol",ylab="Total cholestrol effect")
    lines( Total_chol,effect)
    title(paste("Total cholestrol effects on glucose", format(round(taus[i],2)),"Quantile"))
}
dev.off()

bmi.data %>% subset(Gender == 1) %>% count(RXDDRUG) %>% arrange(desc(n))
# Count number of male 5990 
bmi.data %>% filter(Gender == 'Male')%>%count() 

# Count number of female 6416 
bmi.data %>% filter(Gender == 'Female')%>%count()

# Count number of male  on statin 1185
l=bmi.data %>% filter(Gender == 'Male')  
l=count(l[l$Statin_status==1,]) 
# Count number of Female on statin 1162 
l=bmi.data %>% filter(Gender == 'Female')  
l=count(l[l$Statin_status==1,]) 
# Mean Age for Male 49.9
bmi.data %>% filter(Gender == 'Male')%>% mean(bmi.data$Age)
#error
l=bmi.data[bmi.data$Gender=='Male',] 
mean(l$Age)
# Mean Age for Female 49.73
l=bmi.data[bmi.data$Gender=='Female',] 
mean(l$Age)
# Mean glucose for Male  112.618
l=bmi.data[bmi.data$Gender=='Male',] 
mean(l$Glu)
# Mean glucose for Female 106.47
l=bmi.data[bmi.data$Gender=='Female',] 
mean(l$Glu)



