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
    broom::tidy(se.type = "boot") %>%
    filter(!grepl("factor", term)) %>%
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