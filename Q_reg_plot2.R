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
colnames(bmi.data) <- c("Gender","Age","Race","BMI","Total_chol" ,"Statin_status","Glu","Waist_cir")
bmi.data=na.omit(bmi.data)
bmi.data=bmi.data%>%filter(bmi.data$Age>19)
bmi.data$Gender=recode(bmi.data$Gender, "1" = "Male", "2" = "Female" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

bmi.data$Race=recode(bmi.data$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )
# Classifying population to diabetes and non diabetes

 
#formula <- Glu ~Gender+Age+Race+BMI+Statin_status+Age2+BMI2+Total_chol+Total_col2+Waist_cir 
formula <- Glu ~Gender+Race+BMI+Statin_status+bs(Age,intercept=FALSE,df=5)+bs(Total_chol,intercept=FALSE,df=5)+Waist_cir      
fit3.ols <- summary(lm(formula,data =bmi.data))$coefficients
#attach(bmi.data)
p <- nrow(fit3.ols)
taus <- c(1:4/100, 1:19/20)
fit3 <- array(fit3.ols,c(p,4,length(taus)))
rownames(fit3)=rownames(fit3.ols)
colnames(fit3)=colnames(fit3.ols)
dimnames(fit3)[[3]]=taus
fit3["Age2","Estimate",'0.05']
for(i in 1:length(taus)){
    print(taus[i])
    f <- rq(formula, taus[i], data = bmi.data, method="fn")
    fit3[,,i] <- summary(f)$coefficients
}
##
Value   Std. Error     t value     Pr(>|t|)
(Intercept)                                 225.801759  121.0212519  1.86580254 6.209346e-02
GenderMale                                    2.498428    2.4264868  1.02964826 3.031960e-01
RaceMexican                                   5.427993    7.8351224  0.69277707 4.884629e-01
RaceOther                                    -2.479276    5.4367153 -0.45602465 6.483805e-01
RaceOther_Hispanic                            2.914448    8.7871929  0.33166999 7.401443e-01
RaceWhite                                   -11.088031    5.2101701 -2.12815137 3.334493e-02
BMI                                          -1.458373    0.5172336 -2.81956267 4.816822e-03
Statin_status                                41.849226    4.7152386  8.87531463 0.000000e+00
bs(Age, intercept = FALSE, df = 5)1          -2.915993    8.3973274 -0.34725247 7.284078e-01
bs(Age, intercept = FALSE, df = 5)2           4.747489    8.5615601  0.55451214 5.792388e-01
bs(Age, intercept = FALSE, df = 5)3          54.204479   13.3685990  4.05461175 5.053593e-05
bs(Age, intercept = FALSE, df = 5)4          16.437340   10.1353130  1.62178911 1.048749e-01
bs(Age, intercept = FALSE, df = 5)5          22.981325    7.8594737  2.92402845 3.461815e-03
bs(Total_chol, intercept = FALSE, df = 5)1 -237.388437  156.5634949 -1.51624385 1.294841e-01
bs(Total_chol, intercept = FALSE, df = 5)2 -186.677045  106.6111941 -1.75100791 7.997019e-02
bs(Total_chol, intercept = FALSE, df = 5)3 -272.821027  177.4830618 -1.53716656 1.242790e-01
bs(Total_chol, intercept = FALSE, df = 5)4  511.071281  699.7340439  0.73037933 4.651727e-01
bs(Total_chol, intercept = FALSE, df = 5)5 -227.576988 6573.7807861 -0.03461889 9.723842e-01
Waist_cir                                     1.535725    0.2207446  6.95702084 3.656631e-12
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
#blab <- c("Intercept","Male ","Age","RaceMexican", "RaceOther", "RaceOther_Hispanic ", "White ", "BMI", " Statin_status","Age Suqared","BMI^2","Total cholesterol","Total cholestrol^2","Waist Circumference")
blab <- c("Intercept","Male ","Age","RaceMexican", "RaceOther", "RaceOther_Hispanic ", "White ", "BMI", " Statin_status","Age1","BMI^2","Total cholesterol","Total cholestrol^2","Waist Circumference",
          "Age1","Age2","Age3","Age4","Age5","TC1","TC2","TC3","TC4","TC5","Wasist")

par(mfrow=c(5,4))
for(i in c(1:19)){
    #if(i==1){#adjust intercept to be centercept
     #   Age.bar <- mean(bmi.data$Age)
      #  BMI.bar <- mean(bmi.data$BMI)
      #  b <- fit3[i,1,]+
      #      Age.bar*fit3[3,1,]+(Age.bar^2)*fit3[10,1,]+
      #      BMI.bar*fit3[8,1,]+(BMI.bar^2)*fit3[11,1,]
   # }
   # else{
        b <- fit3[i,1,]
    #}
    
    b.p <- b + qnorm(.95)*fit3[i,2,]
    b.m <- b - qnorm(.95)*fit3[i,2,]
    plot(0:1,range(c(b.m,b.p)),type="n",xlab="",ylab="",cex=.75)
    title(paste(blab[i]),cex=.75)
    polygon(c(taus,rev(taus)),c(b.p,rev(b.m)), col=cols[4])
    points(taus,b, col=cols[3])
    lines(taus,b,col=cols[3])
    abline(h=0)
   # if(i==1){#now fix ols results
        
   #     bhat <- fit3.ols[i,1]+
   #         Age.bar*fit3.ols[3,1]+(Age.bar^2)*fit3.ols[10,1]+
    #        BMI.bar*fit3.ols[8,1]+(BMI.bar^2)*fit3.ols[11,1]
   # }
    #else{
        bhat <- fit3.ols[i,1]
    #}
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
    effect <- fit3[9,1,i]*ages+fit3[10,1,i]*ages^2+fit3[11,1,i]*ages^3+fit3[12,1,i]*ages^4+fit3[13,1,i]*ages^5 
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
bmi.data %>% filter(Gender == 'Male')%>% mean(Age)
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

########

 
######################################################
 
#######
 

###############

library(ggeffects)
library(splines)
data(efc)
fit1 <- lm(barthtot ~ c12hour + bs(neg_c_7) * c161sex + e42dep, data = efc)

fit2=lm( BMI ~ X - 1, data=bmi.data, tau = 0.9)
 
ggpredict(fit1, terms = "Age")

 

 