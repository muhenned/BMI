# Template code of lifetime risk of disease x
# rm(list=ls(all=TRUE))

#Load libraries
library('expm')
library("truncnorm")
library('ggplot2')
library('ggpubr')
library("dplyr")
library("ggpubr")
library("matlib")
library('MortalityLaws')
library("pracma")


# Set constraints
states <-3 # specify number of mutually exlusive states here
min.age <- 20 # Specify minimum age here
max.age <- 79 # specify maximum age here
width <- 1 # specify width of age interval here
ages.1 <- seq(min.age,max.age,width)
nages.1 <- length(ages.1)
a <- seq(states,((max.age+1)-min.age)*states, states)
age.group.rates <- 5 # width of age groups for which you have yoyr rates

age.gps <- length(ages.1)/age.group.rates



###################
### Risk Ratios ###
###################

# Relative risk of dying for OW compared to Normal (replace NA with RR)
RR.m.ow <- rep(1, length(ages.1)) # change to Relative risk from literature


# Relative risk of mortality for OB compared to Normal
RR.m.ob <- rep(1, length(ages.1)) # change to Relative risk from literature


# Relative risk of dying for diabetes cases relative to non-diabetes cases
RR.M.D <- rep(1, length(ages.1)) # change to Relative risk from literature




##############################
####    BMI PREVALENCES   ####
##############################

# Prevalence of overweight in t=1
P.OW <- rep(0.1, length(ages.1)) # change vector to age specific prevalence of overweight

# Prevalence of obesity in t=1
P.OB <- rep(0.4, length(ages.1)) # change vector to age specific prevalence of obesity

# Prevalence of normal weight in t=1
P.N <- 1-(P.OB+P.OW)


##############################
###  DIABETES PREVALENCES  ###
##############################
prev.base1 <- rep(0.1, length(ages.1))  # input age-specific diabetes prevalence
 

p.ow.ob <- P.OB+P.OW
sig1 <- P.OW/p.ow.ob # the proportion of those who are overweight or obese who are overweight


####################
# Parameter values #
####################

# Total pop

# Incidences by age and BMI by whatever age groupings you have (specify from outset) per 1000 PY
i.NW.age.p <- rep(5, ((max.age+1)-min.age)/age.group.rates)/1000     # input incidence rates among normal weight per 1000 PY
i.OW.age.p <- rep(10, ((max.age+1)-min.age)/age.group.rates)/1000     # input incidence rates among overweight per 1000 PY
i.OB.age.p <- rep(15, ((max.age+1)-min.age)/age.group.rates)/1000     # input incidence rates among obese per 1000 PY




seq <- seq((min.age+age.group.rates/2),
           ((max.age+1)-(age.group.rates/2)),
           5) # replce with mid points for the age groups you have
seq2 <- ages.1
  

df <- 3 # specify the degrees of freedom

# input your incidence rates and smooth across desired age groups
inc.norm <- smooth.spline(seq, i.NW.age.p, df = df) ; inc.norm <- predict(inc.norm, seq2)$y
inc.ow <- smooth.spline(seq, i.OW.age.p, df = df) ; inc.ow <- predict(inc.ow, seq2)$y
inc.ob <- smooth.spline(seq, i.OB.age.p, df = df) ; inc.ob <- predict(inc.ob, seq2)$y
# calculate the total incidece as weighted avg of the BMIs
inc.tot <- (inc.norm*P.N) + (inc.ow*P.OW) + (inc.ob*P.OB)


Ages.lt <-c( 0.5, 3, seq(7.5,87.5,5)) # vector of agemidpoints in your lifetable
Mx <- rep(NA, length(Ages.lt)) # vector of age specific death rates to smooth with makeham function or whatver is needed
#### Mx<- lt13$m


# ----- Mortality smooth
Makeham.fit <- MortalityLaw(x=Ages.lt, mx=Mx, law = 'makeham', opt.method = 'LF1')
mort.base <- predict(Makeham.fit, x=min.age:max.age)
mort.tot <- mort.base
mort.norm <- rep(NA, length(mort.base)) # input normal weight specific death rates
mort.ow <- rep(NA, length(mort.base)) # input Overweight specific death rates
mort.ob <- rep(NA, length(mort.base))# input obese specific death rates

mort.norm <- mort.base # input normal weight specific death rates
mort.ow <-mort.base # input Overweight specific death rates
mort.ob <- mort.base# input obese specific death rates



# calculate the diabetes specific death rate by BMI class assuming the diabetes prevalence is constant across bmi classes 
mort.norm.ND <- mort.norm / ((prev.base1*RR.M.D) - prev.base1 +1 )   # Separate mort rates for Norm who are diabetes cases and not diabetes cases
mort.norm.D <- mort.norm.ND * RR.M.D
mort.ow.ND <- mort.ow / ((prev.base1*RR.M.D) - prev.base1 +1 ) # Separate mort rates for ow.ob who are diabetes cases and not diabetes cases
mort.ow.D <- mort.ow.ND * RR.M.D
mort.ob.ND <- mort.ob / ((prev.base1*RR.M.D) - prev.base1 +1 ) # Separate mort rates for ow.ob who are diabetes cases and not diabetes cases
mort.ob.D <- mort.ob.ND * RR.M.D

mort.tot.ND <- mort.tot / ((prev.base1*RR.M.D) - prev.base1 +1 )
mort.tot.D <- mort.tot.ND * RR.M.D


BMI.gp <- length(c("Normal", "Overweight", "Obese" , "Total"))

# row 1= diabetes mort for normal; row 2= diabetes mort for ow; row 3= diabetes mort for ob
mort.D <- matrix(NA, BMI.gp, length(ages.1))

mort.D[1,] <- mort.norm.D
mort.D[2,] <- mort.ow.D
mort.D[3,] <- mort.ob.D
mort.D[4,] <- mort.tot.D
  

# row 2= non-diabetes mort for normal; row 2= non-diabetes mort for ow.ob
mort.ND <- matrix(NA, BMI.gp, length(ages.1))

mort.ND[1,] <- mort.norm.ND
mort.ND[2,] <- mort.ow.ND
mort.ND[3,] <- mort.ob.ND
mort.ND[4,] <- mort.tot.ND


# rows 1-10 = inc normal; 11-20 = inc owob
inc.array <- matrix(NA, BMI.gp, length(ages.1))
inc.array[1,] <- inc.norm
inc.array[2,] <- inc.ow
inc.array[3,] <- inc.ob
inc.array[4,] <- inc.tot
  
inc <- inc.array


a <- seq(states,((max.age+1)-min.age)*states, states)

#####################
# Transition matrix #
#####################
TM <- array(NA, dim=c(((nages.1+1)*states), ((nages.1+1)*states), BMI.gp))

for (s in 1:BMI.gp){
  
  for (i in 1:length(a)){ 
    
    # Row 1 : Transitions for Non-diabetes cases
    TM[(a[i])-2,(a[i])+1,s] <- (-inc[s,a[i]/3]-mort.ND[s,a[i]/3])
    TM[(a[i])-2,(a[i])+2,s] <- inc[s,a[i]/3]
    TM[(a[i])-2,(a[i])+3,s] <- mort.ND[s,a[i]/3]
    
    # Row 2 : Transitions for Diabetes cases
    TM[(a[i])-1,(a[i])+1,s] <- 0
    TM[(a[i])-1,(a[i])+2,s] <- -(mort.D[s,a[i]/3])
    TM[(a[i])-1,(a[i])+3,s] <- mort.D[s,a[i]/3]
    
  }
}  


TM[is.na(TM)]<-0   # Replace the NA with 0
TM.rate <- TM      # TM.rate = rates of transitions  

# Loop for exponentiating the intensity matrix
for(s in 1:BMI.gp){
  for (i in 1:length(a)){ 
    
    mat<-c(TM[a[i]-2,a[i]+1,s] , TM[a[i]-2,a[i]+2,s] , TM[a[i]-2,a[i]+3,s] , 
           0, TM[a[i]-1,a[i]+2,s], TM[a[i]-1,a[i]+3,s], 
           0,0,0)
    
    dim(mat) <- c(3, 3)
    mat<-t(mat)
    mat <- expm(mat)
    
    TM[as.numeric(a[i]-2):as.numeric(a[i]) , as.numeric(a[i]+1) : as.numeric(a[i]+3),s] <- mat
  }
}


######################
# Fundamental matrix #
###################### 
to.del <-seq(3,183,3)      # Deleting the mortality rows and restricting to transcient states
U <- array(NA, dim= c((nages.1+1)*(states-1), (nages.1+1)*(states-1),BMI.gp))        # Transcient matrix 


# delete row and col 3; 6; 9 etc relating to mortality (absorbing states)

for (s in 1:BMI.gp){
  U[,,s] <- TM[-to.del,-to.del,s]
}


I <- diag(1, nrow(U) , ncol(U))     # Identity matrix - dimensions the same as the U

N1 <- array(NA, dim=dim(U))  # Fundamental matrix   

# calculate fundamental matrix for each BMI group
for (s in 1:BMI.gp){
  N1[,,s] <- (inv(I-U[,,s]))
}



#################################
###   LIFETIME RISK OF DIAB   ###
#################################

# Theta = lifetime risk of diabetes
# Matrix for the B=NM calculation - Life time risk of diabetes
e<- seq(1, ((max.age+1)-min.age)*states+3, states)
f <- seq(2, ((max.age+1)-min.age)*states+3, states)
g <- seq(6, ((max.age+1)-min.age)*states+3, states)   # cols for mort rates
M <- array(NA, dim=c(1,(states-1),nages.1,BMI.gp)) # (1) x (diab or Non diab) x (Age) x (resamples)


# creation of the Mortality matrix
for (s in 1:BMI.gp){

    for (i in 1:nages.1){
    M[1,,i,s] <-  TM[e[i]:f[i],g[i],s]
  }  
}


M1 <- matrix(NA, 1, nages.1*2)
M2 <- matrix(NA, 1, nages.1*2)
M3 <- matrix(NA, 1, nages.1*2)
M4 <- matrix(NA, 1, nages.1*2)


# mortality matrices for the 3 BMI groups and total pop
M1[1,] <- as.vector(M[,,,1])
M2[1,] <- as.vector(M[,,,2])
M3[1,] <- as.vector(M[,,,3])
M4[1,] <- as.vector(M[,,,4])


matrix <- array(NA, dim=c((states-1)*(nages.1+1),(states-1)*(nages.1+1),BMI.gp))

matrix[1:(nages.1*2), 1:(nages.1*2), 1] <- diag(M1[,]) #NW
matrix[1:(nages.1*2), 1:(nages.1*2), 2] <- diag(M2[,]) #OW
matrix[1:(nages.1*2), 1:(nages.1*2), 3] <- diag(M3[,]) #OB
matrix[1:(nages.1*2), 1:(nages.1*2), 4] <- diag(M4[,]) #TOT


matrix[121:122, 121:122,] <- diag(1,2,2)
matrix[which(is.na(matrix))]<-0
M <- matrix
B <- array(NA, dim=dim(N1))


########################

# Create a probability distribution of eventual fate

for (s in 1:BMI.gp){
  B[,,s] <- N1[,,s] %*% M[,,s]
}

sum(B[4,,1])   # Beta = occupancy times in each state depending on starting state (proportion of life spent in each state by age)
# sanity check each row of B should sum up to 1 



even <- seq(2,122,2)
odd <- seq(1,122,2)

# creat 2 lifetime risk arrays of dim (1) x (51) x (resamples)
ltr.1 <- matrix(NA, 1,nages.1)
ltr.2 <- matrix(NA, 1,nages.1)
ltr.3 <- matrix(NA, 1,nages.1)
ltr.4 <- matrix(NA, 1,nages.1)


# What is the probability that someone aged x in 2010 and is ND, will die with diab EVENTUALLY
# the lifetime risks of diabetes by each age from 20-79

for (i in 1:nages.1){
  ltr.1[1,i] <- sum(B[(2*i)-1,even,1])
  ltr.2[1,i] <- sum(B[(2*i)-1,even,2])
  ltr.3[1,i] <- sum(B[(2*i)-1,even,3])
  ltr.4[1,i] <- sum(B[(2*i)-1,even,4])
}





