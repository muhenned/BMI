
 
library(quantreg)
library(splines)


#attach(bmi.data)
bmi.data=readRDS(file = "bmi.data.rds")

formula <- Glu ~Gender+Race+BMI+Statin_status+bs(Age,intercept=FALSE,df=5)+bs(Total_chol,intercept=FALSE,df=5)+bs(waist_cir,intercept=FALSE,df=5)      
 
#model <- rq(Glu ~ bs(Age, knots=c(25,50,75))+BMI+Gender, tau=0.5, data=bmi.data ) 

model <- rq(formula, tau=0.5, data=bmi.data ) 
 
 
#plot
# Get model-based predictions
pred <- as.data.frame(predict(model, data.frame(Age = bmi.data$Age, 
    BMI = bmi.data$BMI, Gender = bmi.data$Gender,Race=bmi.data$Race, Statin_status=bmi.data$Statin_status, Total_chol=bmi.data$Total_chol,waist_cir=bmi.data$waist_cir),
                              interval = "confidence"));
pred$Age <- bmi.data$Age;

#plot
sp <- c(25,50,75);
ggplot(bmi.data, aes(x=bmi.data$Age,y=bmi.data$Glu)) +
    #geom_point() +
    geom_line(data = pred, aes(x = Age, y = fit)) +
    geom_ribbon(data = pred, aes(ymin = lower, ymax = higher, x = Age), alpha = 0.4) +
    ylim(70,400)+
    geom_quantile(formula = bmi.data$Glu ~ bs(bmi.data$Age,intercept=FALSE,df=5), quantiles = 0.5, se = T);









################################################################################################

#create data    
x <- seq(0,100,length.out = 100)        
sig <- 0.1 + 0.05*x 
b_0 <- 6                                
b_1 <- 0.1                              
set.seed(1)                             
e <- rnorm(100,mean = 0, sd = sig)      
y <- b_0 + b_1*x + e 

mydata <- data.frame(x,y, age=sample(30:70,100,replace=TRUE), sex=sample(c("Male","Female"),100, replace=TRUE))

#run regression
library(quantreg)
library(splines)
model <- rq(y ~ ns(x, knots=c(25,50,75))+age+sex, tau=0.5, data=mydata ) 

#plot
#sp <- c(25,50,75)
#ggplot(mydata, aes(x=x,y=y))+ geom_point()+ geom_quantile(formula=y~ns(x,knots=sp), quantiles=0.5, se=T)


# Get model-based predictions
pred <- as.data.frame(predict(model, data.frame(x = mydata$x, age = mydata$age, sex = mydata$sex), interval = "confidence"));
pred$x <- mydata$x;

#plot
sp <- c(25,50,75);
ggplot(mydata, aes(x=x,y=y)) +
    geom_point() +
    geom_line(data = pred, aes(x = x, y = fit)) +
    geom_ribbon(data = pred, aes(ymin = lower, ymax = higher, x = x), alpha = 0.4) +
    geom_quantile(formula = y ~ ns(x, knots = sp), quantiles = 0.5, se = T);
