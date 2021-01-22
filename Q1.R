library(quantreg)
library(ggplot2)
library(dplyr)
 
# OLS
lm <- lm(data=mtca,
         formula =  hp ~  disp + mpg + I(mpg^2) + qsec + am)

ols <- as.data.frame(coef(lm))
ols.ci <- as.data.frame(confint(lm))
ols2 <- cbind(ols, ols.ci)
ols2 <- tibble::rownames_to_column(ols2, var="term")

# Quantile
l=rq(data=mtcars,
   tau= 1:9/10,
   formula = hp ~  disp + mpg + I(mpg^2) + qsec + am) %>%
    broom::tidy(se.type = "boot")


l$std.error[1]
################
l=rq(data=mtcars, 
   tau= 1:9/10,
   formula = hp ~  disp + mpg + I(mpg^2) + qsec + am) %>%
    broom::tidy(se.type = "boot") %>%
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
######

cyls=unique(mtcars$cyl)
n_groups=length(cyls)
sample_mean=rep(NA,n_groups)
cis=matrix(nrow = n_groups,ncol = 2)
for(i in 1:n_groups){
 # extract relavent data
    rows=which(mtcars$cyl==cyls[i])
    obser=mtcars$mpg[rows]
    #store sample mean
    sample_mean[i]=mean(obser)
    # contruct CI
    stdev=sd(obser)
    n=length(obser)
    se_mean=stdev/sqrt(n)
    #store CI
    cis[i,1]=sample_mean[i]-2*se_mean
    cis[i,2]=sample_mean[i]+2*se_mean
    }


library(quantreg)
library(ggplot2)
library(dplyr)

# OLS
lm <- lm(data=mtcars,
         formula =  hp ~  disp + mpg + I(mpg^2) + qsec + am)

ols <- as.data.frame(coef(lm))
ols.ci <- as.data.frame(confint(lm))
ols2 <- cbind(ols, ols.ci)
ols2 <- tibble::rownames_to_column(ols2, var="term")

# Quantile
rq(data=mtcars,
   tau= 1:9/10,
   formula = hp ~  disp + mpg + I(mpg^2) + qsec + am) %>%
    broom::tidy(se.type = "boot")
