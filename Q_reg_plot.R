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
for(i in 1:length(taus)){
    print(taus[i])
    f <- rq(formula, taus[i], data = bmi.data, method="fn")
    fit3[,,i] <- summary(f)$coefficients
}
##
(Intercept)        194.238571676 64.666035998  3.0037185 2.672537e-03
GenderMale           3.647848794  3.155954251  1.1558624 2.477605e-01
Age                  1.326574494  0.465484350  2.8498799 4.381047e-03
RaceMexican          9.228608538  8.558627551  1.0782814 2.809300e-01
RaceOther           -4.151100984  6.144220397 -0.6756107 4.993009e-01
RaceOther_Hispanic   1.275710479  8.781682930  0.1452695 8.845006e-01
RaceWhite          -14.390799248  5.627954833 -2.5570211 1.056951e-02
BMI                 -4.380820563  1.552191187 -2.8223460 4.775236e-03
Statin_status       44.861806366  6.279534638  7.1441291 9.583445e-13
Age2                -0.007880983  0.004873444 -1.6171281 1.058770e-01
BMI2                 0.055172291  0.021618604  2.5520747 1.072067e-02
Total_chol          -1.710974019  0.658651748 -2.5976915 9.396776e-03
Total_col2           0.004551597  0.001757476  2.5898493 9.613434e-03
Waist_cir            1.488463085  0.287258378  5.1816177 2.235505e-07
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
 
par(mfrow=c(4,3))
for(i in c(1:11)){
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



