# Attempt to reproduce birthweight fit  May 2011
require(quantreg)
load("natality.Rda")
d <- cbind(d, m.wtgain2 = d$m.wtgain^2)
formula <- weight ~ black+married+boy+tri2+tri3+novisit+ed.hs+ed.smcol+
    ed.col+mom.age+smoke+cigsper+m.wtgain+mom.age2+m.wtgain2
fit3.ols <- summary(lm(formula,data = d))$coefficients
p <- nrow(fit3.ols)
taus <- c(1:4/100, 1:19/20)
fit3 <- array(fit3.ols,c(p,4,length(taus)))
for(i in 1:length(taus)){
    print(taus[i])
    f <- rq(formula, taus[i], data = d, method="fn")
    fit3[,,i] <- summary(f)$coefficients
}


#########################################################################################

#Plot results from newfit.R:
#Formula:
#weight ~ black + married + boy + tri2 + tri3 + novisit + ed.hs + ed.smcol +
#        ed.col + mom.age + smoke + cigsper + m.wtgain + mom.age2 + m.wtgain^2
# Use these colors for PostScript on the next device startup.
cols <- c("black","red", "blue","LightSkyBlue1", "green", "light pink")
pdf("newfig.pdf",width=8.5,height=7)
p <- dim(fit3)[1]
blab <- c("Intercept", "Black", "Married", "Boy", "Prenatal Second", 
          "Prenatal Third", "No Prenatal", "High School", "Some College", "College", 
          "Mother's Age", "Smoker", "Cigarette's/Day", "Mother's Weight Gain", 
          "Mother's Age^2","Mother's Weight Gain^2")
attach(d)
par(mfrow=c(2,4))
for(i in c(1,4,3,2,11,15,8,9,10,7,5,6,12,13,14,16)){
    if(i==1){#adjust intercept to be centercept
        mom.age.bar <- mean(mom.age)
        m.wtgain.bar <- mean(m.wtgain)
        b <- fit3[i,1,]+
            mom.age.bar*fit3[11,1,]+(mom.age.bar^2)*fit3[15,1,]+
            m.wtgain.bar*fit3[14,1,]+(m.wtgain.bar^2)*fit3[16,1,]
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
            mom.age.bar*fit3.ols[11,1]+(mom.age.bar^2)*fit3.ols[15,1]+
            m.wtgain.bar*fit3.ols[14,1]+(m.wtgain.bar^2)*fit3.ols[16,1]
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
#Now try to plot marginal effect of mother's weight gain for several gains
pdf("newfig2.pdf",width=7.0,height=7.0)
par(mfrow=c(2,2))
gains <- quantile(m.wtgain,c(.1,.25,.75,.9))
for(i in 1:length(gains)){
    effect <- fit3[14,1,]+2*fit3[16,1,]*gains[i]
    plot(taus,effect,xlab="Quantile",ylab="Weight Gain Effect")
    lines(taus,effect)
    #title(paste("Mother's Weight Gain", format(round(gains[i])),"Lbs"))
}
dev.off()
#Now try to plot quadratic effect of mother's age for several taus
pdf("newfig3.pdf",width=7.0,height=7.0)
par(mfrow=c(2,2))
ages <- seq(min(mom.age),max(mom.age),by=1)
for(i in c(2,5,15,18)){
    effect <- fit3[11,1,i]*ages+fit3[15,1,i]*ages^2
    plot(ages,effect,type="n",xlab="Mother's Age",ylab="Weight Gain Effect")
    lines(ages,effect)
    #title(paste("Weight Gain Effect at", format(round(taus[i],2)),"Quantile"))
}
dev.off()
