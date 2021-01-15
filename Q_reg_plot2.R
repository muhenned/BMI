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

 
#formula <- Glu ~Gender+Age+Race+BMI+Statin_status+Age2+BMI2+Total_chol+Total_col2+Waist_cir 
formula <- Glu ~Gender+Race+BMI+Statin_status+bs(Age,intercept=FALSE,df=5)+bs(Total_chol,intercept=FALSE,df=5)+bs(waist_cir,intercept=FALSE,df=5)      
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
 
##########################################################
########################################################################
#########################################################


#Plot results from newfit.R:
#Formula:
# Use these colors for PostScript on the next device startup.
cols <- c("black","red", "blue","LightSkyBlue1", "green", "light pink")
#pdf("newfig.pdf",width=8.5,height=7)
#pdf("./images/newfig.pdf",width=15,height=13)
png(file = here::here("images", "newfig11.png"),
    res = 400, height = 9, width = 16, units = "in")
p <- dim(fit3)[1]
#blab <- c("Intercept","Male ","Age","RaceMexican", "RaceOther", "RaceOther_Hispanic ", "White ", "BMI", " Statin_status","Age Suqared","BMI^2","Total cholesterol","Total cholestrol^2","Waist Circumference")
blab <- c("Intercept","Male ","Age","RaceMexican", "RaceOther", "RaceOther_Hispanic ", "White ", "BMI", " Statin_status","Age1","BMI2","Total cholesterol","Total cholestrol2","Waist Circumference",
          "Age1","Age2","Age3","Age4","Age5","TC1","TC2","TC3","TC4","TC5","Wasist1","Wasist2","Wasist3","Wasist4","Wasist5")
plot(summary(fit3))
par(mfrow=c(6,4))
for(i in c(1:23)){
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
png(file = here::here("images", "newfig22.png"),
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
png(file = here::here("images", "newfig33.png"),
    res = 400, height = 9, width = 16, units = "in")
# construct the b-spline expansion on the generated ages

par(mfrow=c(2,2))
# extract the splines from the model fit
age_bs <- bs(bmi.data$Age,intercept=FALSE,df=5)

ages <- seq(min(bmi.data$Age),max(bmi.data$Age),by=1)

ages_bs <- bs(ages, degree = attr(age_bs, "degree"), knots = attr(age_bs, "knots"),
              Boundary.knots = attr(age_bs, "Boundary.knots"), intercept = FALSE)

for(i in c(6,9,15,22)){
    effect <- fit3[9,1,i]*ages_bs[, 1] + fit3[10,1,i] * ages_bs[, 2] + fit3[11,1,i] * ages_bs[, 3] +
        fit3[12,1,i] * ages_bs[, 4] + fit3[13,1,i] * ages_bs[, 5]
    plot(ages,effect,type="n",xlab="Age",ylab="Age effect")
    lines(ages,effect)
     title(paste("Age effects on glucose", format(round(taus[i],2)),"Quantile"))
}
dev.off()

#Total cholestrol effects on glucose.

png(file = here::here("images", "newfig44.png"),
    res = 400, height = 9, width = 16, units = "in")

par(mfrow=c(2,2))
# extract the splines from the model fit
Total_Chol_bs <- bs(bmi.data$Total_chol,intercept=FALSE,df=5)
Total_chol <- seq(min(bmi.data$Total_chol),300,by=1)

Total_Chol_bs <- bs(Total_chol, degree = attr(Total_Chol_bs , "degree"), knots = attr(Total_Chol_bs, "knots"), 
                  Boundary.knots = attr(Total_Chol_bs, "Boundary.knots"), intercept = FALSE)


for(i in c(6,9,15,22)){
    effect <- fit3[14,1,i]*Total_Chol_bs[, 1] + fit3[15,1,i] *Total_Chol_bs[, 2] + fit3[16,1,i] * Total_Chol_bs[, 3] + fit3[17,1,i] * Total_Chol_bs[, 4] +
        fit3[18,1,i] * Total_Chol_bs[, 5]  
    plot(Total_chol,effect,type="n",xlab="Total cholestrol",ylab="Total cholestrol effect")
    lines( Total_chol,effect)
    title(paste("Total cholestrol effects on glucose", format(round(taus[i],2)),"Quantile"))
}
dev.off()


#Waist circumference  effects on glucose.

png(file = here::here("images", "newfig55.png"),
    res = 400, height = 9, width = 16, units = "in")

par(mfrow=c(2,2))
# extract the splines from the model fit
waist_cir_bs <- bs(bmi.data$waist_cir,intercept=FALSE,df=5)
waist_cir <- seq(min(bmi.data$waist_cir),150,by=1)

waist_cir_bs <- bs(waist_cir, degree = attr(waist_cir_bs , "degree"), knots = attr(waist_cir_bs, "knots"), 
                    Boundary.knots = attr(waist_cir_bs, "Boundary.knots"), intercept = FALSE)


for(i in c(6,9,15,22)){
    effect <- fit3[19,1,i]* waist_cir_bs[, 1] + fit3[20,1,i] *waist_cir_bs[, 2] + fit3[21,1,i] * waist_cir_bs[, 3] + fit3[22,1,i] * waist_cir_bs[, 4] +
        fit3[23,1,i] * waist_cir_bs[, 5]  
    plot(waist_cir,effect,type="n",xlab="Total cholestrol",ylab="Total cholestrol effect")
    lines( waist_cir,effect)
    title(paste("Waist circumference effects on glucose", format(round(taus[i],2)),"Quantile"))
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

#############################################
################################################################
##############################################

# extract the splines from the model fit
age_bs <- bs(bmi.data$Age,intercept=FALSE,df=5)
# generate a sequence of ages
ages <- seq(min(bmi.data$Age),max(bmi.data$Age),by=1)
# construct the b-spline expansion on the generated ages
ages_bs <- bs(ages, degree = attr(age_bs, "degree"), knots = attr(age_bs, "knots"), Boundary.knots = attr(age_bs, "Boundary.knots"), intercept = FALSE)

for(i in c(6,9,15,22)){
    # evaluate the spline basis times the coefficients
    effect <- fit3[9,1,i]*ages_bs[, 1] + fit3[10,1,i] * ages_bs[, 2] + fit3[11,1,i] * ages_bs[, 3] + fit3[12,1,i] * ages_bs[, 4] + fit3[13,1,i] * ages_bs[, 5]
    plot(ages,effect,type="n",xlab="Age",ylab="Age effect")
    lines(ages,effect)
    title(paste("Age effects on glucose", format(round(taus[i],2)),"Quantile"))
}

# Another form for effect calculation
#effect <- ages_bs %*% fit3[9:13, 1, i]

########################################################
##################################################################################
########################################################

 library(splines)

 plot(bmi.data$Age,bmi.data$Total_chol,xlab = "milliseconds", ylab = "acceleration",type="n")
 points(bmi.data$Age,bmi.data$Total_chol,cex = .75)
 X <- model.matrix(bmi.data$Glu ~ bs(bmi.data$Age, df=15)+bs(bmi.data$Total_chol,df=15)+bmi.data$Gender)
 for(tau in 1:3/4){
     fit <- rq(Glu ~ bs(Age, df=15)+bs(Total_chol,df=15)+Gender , tau=tau, data=bmi.data)
     accel.fit <- X %*% fit$coef
     plot(bmi.data$Age,accel.fit)
     }

 ###########
 plot(times,accel,xlab = "milliseconds", ylab = "acceleration",type="n")
 points(times,accel,cex = .75)
 age_bs <- bs(times,intercept=FALSE,df=15)
 # generate a sequence of ages
 ages <- seq(min(bmi.data$Age),max(bmi.data$Age),by=1)
 # construct the b-spline expansion on the generated ages
 ages_bs <- bs(ages, degree = attr(age_bs, "degree"), knots = attr(age_bs, "knots"), Boundary.knots = attr(age_bs, "Boundary.knots"), intercept = FALSE)
 
 for(i in c(6,9,15,22)){
     # evaluate the spline basis times the coefficients
     effect <- fit3[9,1,i]*ages_bs[, 1] + fit3[10,1,i] * ages_bs[, 2] + fit3[11,1,i] * ages_bs[, 3] + fit3[12,1,i] * ages_bs[, 4] + fit3[13,1,i] * ages_bs[, 5]
     plot(ages,effect,type="n",xlab="Age",ylab="Age effect")
     lines(ages,effect)
     title(paste("Age effects on glucose", format(round(taus[i],2)),"Quantile"))
 }
 
 
 
  
 library(splines)
  library(MASS)
  data(mcycle)
  attach(mcycle)
  plot(bmi.data$Total_chol,bmi.data$Glu,xlab = "milliseconds", ylab = "acceleration",type="n")
  points(bmi.data$Total_chol,bmi.data$Glu,cex = .75)
  X <-model.matrix(bmi.data$Glu ~ bs(bmi.data$Total_chol, df=3))
  for(tau in 1:1/4){
      fit <- rq(Glu ~ bs(Total_chol, df=3), tau=tau, data=bmi.data)
      accel.fit <- X %*% fit$coef
      #accel.fit<- predict(fit,newdata =X,interval = "confidence")
      lines(bmi.data$Total_chol,accel.fit[,1])
  }
  
  attach(mcycle)
  plot(bmi.data$Waist_cir,bmi.data$Glu,xlab = "milliseconds", ylab = "acceleration",type="n")
  points(bmi.data$Waist_cir,bmi.data$Glu,cex = .75)
  X <-model.matrix(bmi.data$Glu ~ bs(bmi.data$Total_chol, df=3))
  for(tau in 1:3/4){
      fit <- rq(Glu ~ bs(Waist_cir, df=3), tau=tau, data=bmi.data)
      accel.fit <- X %*% fit$coef
      #accel.fit<- predict(fit,newdata =X,interval = "confidence")
      lines(bmi.data$Total_chol,accel.fit[,1])
  }
###########
  #############
  ##############
  png(file = here::here("images", "newfig11.png"),
      res = 400, height = 9, width = 16, units = "in")
  par(mfrow=c(7,4))
  quantreg.all <- rq(formula, tau = seq(0.05, 0.95, by = 0.05), data=bmi.data)
  
  quantreg.plot <- summary(quantreg.all)
  
  plot(quantreg.plot)
  dev.off()
 
  