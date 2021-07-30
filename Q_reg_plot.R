# Plot of quantile regression for BMI regressed  Gender, Age,Race,Cholesterol_Drug_Use, Total cholestrol
#Roger Koenker
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
colnames(bmi.data) <- c("Gender","Age","Race","BMI","Total_chol" ,"Cholesterol_Drug_Use","Glu","waist_cir")
bmi.data=na.omit(bmi.data)
bmi.data=bmi.data%>%filter(bmi.data$Age>19)
bmi.data$Gender=recode(bmi.data$Gender, "1" = "Male", "2" = "Female" )
# RIDRETH1 (race, 1=mexican, 2=otherHispanic, 3= Non-Hispanic White,4=Non-Hispanic Black ,5=Other Race - Including Multi-Racial

bmi.data$Race=recode(bmi.data$Race, "1" = "Mexican", "2" = "Other_Hispanic","3"="White","4"="Black","5"="Other" )


# Save an object to a file
saveRDS(bmi.data, file = "bmi.data.rds")
# Restore the object
bmi.data=readRDS(file = "bmi.data.rds")


bmi.data <- cbind(bmi.data, Age2 = bmi.data$Age^2)

bmi.data <- cbind(bmi.data, Total_chol2 = bmi.data$Total_chol^2)

formula <- BMI ~Gender+Race+Cholesterol_Drug_Use+Age+Age2+Total_chol+Total_chol2 

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
Value   Std. Error      t value     Pr(>|t|)
(Intercept)         4.032594e+01 2.7295368597  14.77391451 0.000000e+00
GenderMale         -3.158801e+00 0.3693477977  -8.55237577 0.000000e+00
RaceMexican        -2.746026e+00 0.7487990714  -3.66724016 2.462451e-04
RaceOther          -8.159065e+00 0.6077345693 -13.42537585 0.000000e+00
RaceOther_Hispanic -4.308180e+00 0.5900019175  -7.30197616 3.015366e-13
RaceWhite          -1.562812e+00 0.5240149186  -2.98238161 2.865916e-03
Cholesterol_Drug_Use       1.552109e+00 0.4588676563   3.38247697 7.206547e-04
Age                 4.241028e-01 0.0619080374   6.85052882 7.718270e-12
Age2               -4.958502e-03 0.0005839960  -8.49064439 0.000000e+00
Total_chol         -1.533772e-02 0.0233963352  -0.65556097 5.121192e-01
Total_col2         -3.066944e-06 0.0000543447  -0.05643501 9.549962e-01
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
    res = 400, height = 16, width = 16, units = "in")
p <- dim(fit3)[1]
blab <- c("Intercept","Male ","RaceMexican", "RaceOther", "RaceOther_Hispanic ", "White ", " Cholesterol_Drug_Use","Age","Age^2","Total cholesterol","Total cholestrol^2")

par(mfrow=c(4,3))
for(i in c(1:11)){
  if(i==1){#adjust intercept to be centercept
    Age.bar <- mean(bmi.data$Age)
    Total_chol.bar <- mean(bmi.data$Total_chol)
    b <- fit3[i,1,]+
      Age.bar*fit3[8,1,]+(Age.bar^2)*fit3[9,1,]+
      Total_chol.bar*fit3[10,1,]+(Total_chol.bar^2)*fit3[11,1,]
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
      Age.bar*fit3.ols[8,1]+(Age.bar^2)*fit3.ols[9,1]+
      Total_chol.bar*fit3.ols[10,1]+(Total_chol.bar^2)*fit3.ols[11,1]
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

#Now try to plot quadratic effect of age on BMI

#pdf("newfig3.pdf",width=7.0,height=7.0)
png(file = here::here("images", "p_age_effect.png"),
    res = 400, height = 9, width = 16, units = "in")

par(mfrow=c(2,2))
ages <- seq(min(bmi.data$Age),max(bmi.data$Age),by=1)
for(i in c(6,9,15,22)){
  effect <- fit3[8,1,i]*ages+fit3[9,1,i]*ages^2
  plot(ages,effect,type="n",xlab="Age",ylab="Age effect")
  lines(ages,effect)
  title(paste("Age effects on BMI", format(round(taus[i],2)),"Quantile"))
}
dev.off()

#Total cholesterol effects on glucose.

png(file = here::here("images", "p_TC_effect.png"),
    res = 400, height = 9, width = 16, units = "in")

par(mfrow=c(2,2))
Total_chol <- seq(min(bmi.data$Total_chol),300,by=1)
for(i in c(6,9,15,22)){
  effect <- fit3[10,1,i]*Total_chol +fit3[11,1,i]*Total_chol^2
  plot(Total_chol,effect,type="n",xlab="Total cholestrol",ylab="Total cholestrol effect")
  lines( Total_chol,effect)
  title(paste("Total cholestrol effects on BMI", format(round(taus[i],2)),"Quantile"))
}
dev.off()

bmi.data %>% subset(Gender == 1) %>% count(RXDDRUG) %>% arrange(desc(n))
# Count number of male 5990 
bmi.data %>% filter(Gender == 'Male')%>%count() 

# Count number of female 6416 
bmi.data %>% filter(Gender == 'Female')%>%count()

# Count number of male  on statin 1185
l=bmi.data %>% filter(Gender == 'Male')  
l=count(l[l$Cholesterol_Drug_Use==1,]) 
# Count number of Female on statin 1162 
l=bmi.data %>% filter(Gender == 'Female')  
l=count(l[l$Cholesterol_Drug_Use==1,]) 
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
######Mean BMI Female
l=bmi.data[bmi.data$Gender=='Female',] 
mean(l$BMI)
######Mean BMI Male
l=bmi.data[bmi.data$Gender=='Male',] 
mean(l$BMI)
############################################################



#plot for marginal effect of To for several gains

png(file = here::here("images", ".png"),
    res = 400, height = 9, width = 16, units = "in")
par(mfrow=c(2,2))
gains <- quantile(bmi.data$Total_chol,c(.1,.25,.75,.9))
for(i in 1:length(gains)){
  effect <- fit3[8,1,]+2*fit3[9,1,]*gains[i]
  plot(taus,effect,xlab="Quantile",ylab="Weight Gain Effect")
  lines(taus,effect)
  title(paste("Weight Gain", format(round(gains[i])),"Lbs"))
}
dev.off()


########################


#plot for marginal effect of Total Cgolestrol  for several values 

png(file = here::here("images", "taus_TC.png"),
    res = 400, height = 9, width = 16, units = "in")
par(mfrow=c(2,2))
gains <- quantile(bmi.data$Total_chol,c(.1,.25,.75,.9))
for(i in 1:length(gains)){
  effect <- fit3[10,1,]+2*fit3[11,1,]*gains[i]
  plot(taus,effect,xlab="Quantile",ylab="Weight Gain Effect")
  lines(taus,effect)
  title(paste("Total Cholestrol", format(round(gains[i])),"Lbs"))
}
dev.off()


###################
##########################

###############Trash


par(mfrow=c(2,2))
ages <- seq(min(bmi.data$Age),max(bmi.data$Age),by=1)
for(i in c(6,9,15,22)){
  effect <- fit3[9,1,i]*ages+fit3[10,1,i]*ages^2
  b=effect
  b.p <- b + qnorm(.95)*fit3[9,2,]
  b.m <- b - qnorm(.95)*fit3[9,2,]
  plot(0:1,range(c(b.m,b.p)),type="n",xlab="",ylab="",cex=.75)
  #title(paste(blab[i]),cex=.75)
  #polygon(c(ages,rev(ages)),c(b.p,rev(b.m)), col=cols[4])
  points(ages,b, col=cols[3])
  lines(ages,b,col=cols[3])
  abline(h=0)
  #plot(ages,effect,type="n",xlab="Age",ylab="Age effect")
  lines(ages,effect)
  #title(paste("Age effects on glucose", format(round(taus[i],2)),"Quantile"))
}

for(i in c(1:11)){
  b <- fit3[i,1,]
  b.p <- b + qnorm(.95)*fit3[i,2,]
  b.m <- b - qnorm(.95)*fit3[i,2,]
  plot(0:1,range(c(b.m,b.p)),type="n",xlab="",ylab="",cex=.75)
  title(paste(blab[i]),cex=.75)
  polygon(c(taus,rev(taus)),c(b.p,rev(b.m)), col=cols[4])
  points(taus,b, col=cols[3])
  lines(taus,b,col=cols[3])
  abline(h=0)
  
  
  
  ####################3/11
  