# Plot of quantile regression for BMI regressed on Gender, Age,Race,Cholesterol_Drug_Use
#Using bsplines
require(quantreg)
library(tidyverse)
library(splines)

bmi.data=readRDS(file = "bmi.data.rds")


#formula <- Glu ~Gender+Age+Race+BMI+Cholesterol_Drug_Use+Age2+BMI2+Total_chol+Total_col2+Waist_cir 
formula <- BMI ~Gender+Race+Cholesterol_Drug_Use+bs(Age,intercept=FALSE,df=5)+bs(Total_chol,intercept=FALSE,df=5)       
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
#blab <- c("Intercept","Male ","Age","RaceMexican", "RaceOther", "RaceOther_Hispanic ", "White ", "BMI", " Cholesterol_Drug_Use","Age Suqared","BMI^2","Total cholesterol","Total cholestrol^2","Waist Circumference")
blab <- c("Intercept","Male ","RaceMexican", "RaceOther", "RaceOther_Hispanic ", "White ", " Cholesterol_Drug_Use" ,
          "Age1","Age2","Age3","Age4","Age5","TC1","TC2","TC3","TC4","TC5")

par(mfrow=c(6,4))
for(i in c(1:17)){
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



###########################
#Or we can get this plot using quant regression summary plot.
########################################################################




png(file = here::here("images", "newfig1100.png"),
    res = 400, height = 9, width = 16, units = "in")
par(mfrow=c(7,4))
quantreg.all <- rq(formula, tau = seq(0.05, 0.95, by = 0.05), data=bmi.data)

quantreg.plot <- summary(quantreg.all) 
plot(quantreg.plot)
dev.off()










#Now try to plot b splines effects of age on BMI for several taus

png(file = here::here("images", "s_age_effect.png"),
    res = 400, height = 9, width = 16, units = "in")
# construct the b-spline expansion on the generated ages

par(mfrow=c(2,2))
# extract the splines from the model fit
age_bs <- bs(bmi.data$Age,intercept=FALSE,df=5)

ages <- seq(min(bmi.data$Age),max(bmi.data$Age),by=1)

ages_bs <- bs(ages, degree = attr(age_bs, "degree"), knots = attr(age_bs, "knots"),
              Boundary.knots = attr(age_bs, "Boundary.knots"), intercept = FALSE)

for(i in c(6,9,15,22)){
  effect <- fit3[9,1,i]*ages_bs[, 1] + fit3[9,1,i] * ages_bs[, 2] + fit3[10,1,i] * ages_bs[, 3] +
    fit3[11,1,i] * ages_bs[, 4] + fit3[12,1,i] * ages_bs[, 5]
  plot(ages,effect,type="n",xlab="Age",ylab="Age effect")
  lines(ages,effect)
  title(paste("Age effects on BMI", format(round(taus[i],2)),"Quantile"))
}
dev.off()




# Cholestrol effect on BMI

png(file = here::here("images", "s_TC_effect.png"),
    res = 400, height = 9, width = 16, units = "in")
#Total cholestrol effects on BMI.



par(mfrow=c(2,2))
# extract the splines from the model fit
Total_chol_bs <- bs(bmi.data$Total_chol,intercept=FALSE,df=5)
Total_chol <- seq(min(bmi.data$Total_chol),300,by=1)

Total_chol_bs <- bs(Total_chol, degree = attr(Total_chol_bs , "degree"), knots = attr(Total_chol_bs, "knots"), 
                    Boundary.knots = attr(Total_chol_bs, "Boundary.knots"), intercept = FALSE)
QR.b <- boot.rq(cbind(1,Total_chol_bs),bmi.data$BMI,tau=0.5, R=10000)

for(i in c(6,9,15,22)){
  effect <- fit3[13,1,i]*Total_chol_bs[, 1] + fit3[14,1,i] *Total_chol_bs[, 2] + fit3[15,1,i] * Total_chol_bs[, 3] + fit3[16,1,i] * Total_chol_bs[, 4] +
    fit3[17,1,i] * Total_chol_bs[, 5]  
  plot(Total_chol,effect,type="n",xlab="Total cholestrol",ylab="Total cholestrol effect")
  lines( Total_chol,effect)
  #for(j in seq_len(nrow(QR.b$B))) {
  #    line(QR.b$B[j,1], QR.b$B[j,2] )
  #}
  title(paste("Total cholestrol effects on BMI", format(round(taus[i],2)),"Quantile"))
}
dev.off()



#####################################



