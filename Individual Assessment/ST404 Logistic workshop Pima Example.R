
################################
#                              #
#       BLOCK I                #
#  Check and install packages  #
#                              #
################################

# check and install required packages
# Needed if you wish to use the Anova function 
#(different to the anova function Note capital "A")
#Needed for influenceindes plot
if(!require(car)){
  install.packages("car")
  library(car)
}
# needed for data
if(!require(MASS)){
  install.packages("MASS")
  library(MASS)
}
# needed for emplogitplot1 function
if(!require(Stat2Data)){
  install.packages("Stat2Data")
  library(Stat2Data)
}
#needed for count function
if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}
#needed for half normal plot
if(!require(hnp)){
  install.packages("hnp")
  library(hnp)
}
#needed for binned residualplots
if(!require(arm)){
  install.packages("arm")
}
#needed for cross validation
if(!require(DAAG)){
  install.packages("DAAG")
  library(DAAG)
}
# needed for effects plots.
if(!require(effects)){
  install.packages("effects")
  library(effects)
}
# needed for plot of odds ratios plot_model function
if(!require(sjPlot)){
install.packages("sjPlot")
library(sjPlot)
}
# needed for basic ROC curves
if(!require(ROCR)){
  install.packages("ROCR")
  library(ROCR)
}
# needed for sophiticated ROC and bootstrap interval
if(!require(pROC)){
  install.packages("pROC")
  library(pROC)
}
# avoid scientific display of numbers (run if preferred)
options(scipen = 6)
options(digits=3)

################################
#                              #
#       BLOCK II               #
#       Prepare Data           #
#                              #
################################
# data avaialbe with MASS package, comes in two samples
data("Pima.te")
data("Pima.tr")
#For purpose of illustration we want a lot of rows of data so join 
Pima <- rbind(Pima.te,Pima.tr)
attach(Pima)  # attach data to avoid having to type in two level name  e.g. Pima$npreg
################################
#                              #
#       BLOCK III              #
#       Plot Data: Univariate  #
#                              #
################################
#Set some graphical parameters
# move axis labels and titles closer (mgp) and make margins smaller (mar)
# Plot in a grid that is 3x3 (mfrow)
par(mfrow=c(2,4),mgp=c(1.5,0.5,0),mar=c(3,3,1.2,0.5))  # change grid size
barplot(prop.table(summary(type)),col="blue",main="Bar Chart of Diabetes Test",
        ylab="",xlab="Result of Test for diabetes", ylim=c(0,0.7),
        axis.lty=1)
mtext("Proportion of Women in Study",side=2,line=1.7)
hist(age,breaks=20,col="blue",main="Histogram of Age",xaxt= "n",yaxt= "n",probability = T,ylim=c(0,0.1),xlim=c(20,90))
axis(side=1,at=seq(20,90,5),labels=seq(20,90,5),pos=0)
axis(side=2,at=seq(0,0.1,0.02),labels=seq(0,0.1,0.02),pos=20,las=2)
barplot(prop.table(summary(as.factor(npreg))),col="blue",main="Plot of Number of Pregnacies",
        xlab="",ylab="Proportion of Women in Study",ylim=c(0,0.25),axis.lty=1,yaxt= "n")
axis(side=2,at=seq(0,0.25,0.05),labels=seq(0,0.25,0.05),pos=0,las=2)
mtext("Number of Pregnacies",side=1,line=1.7)
hist(glu,breaks=20,col="blue",main="Histogram of Glucose Concentration",probability = T,
     xlab="Glucose Concentration (mg/dL)",ylim=c(0,0.015),xlim=c(50,210),xaxt= "n",yaxt= "n")
axis(side=1,at=seq(50,210,20),labels=seq(50,210,20),pos=0)
axis(side=2,at=seq(0,0.015,0.005),labels=seq(0,0.015,0.005),pos=50,las=2)
hist(bp,breaks=20,col="blue",main="Histogram of Diastolic Blood Pressure",probability = T
     ,xlab="Diastolic Blood Pressure (mm Hg)",ylim=c(0,0.04),xlim=c(20,120),xaxt= "n",yaxt= "n")
axis(side=1,at=seq(20,120,20),labels=seq(20,120,20),pos=0)
axis(side=2,at=seq(0,0.04,0.005),labels=seq(0,0.04,0.005),pos=20,las=2/C)
hist(skin,breaks=20,col="blue",main="Histogram of Triceps Skin Thickness",probability = T,
     xlab="Triceps Skin Fold Thickness (mm)",ylim=c(0,0.05),xlim=c(0,100),xaxt= "n",yaxt= "n")
axis(side=1,at=seq(0,100,20),labels=seq(0,100,20),pos=0)
axis(side=2,at=seq(0,0.05,0.01),labels=seq(0,0.05,0.01),pos=0,las=2)
hist(bmi,breaks=20,col="blue",main="Histogram of Body mass Index",probability = T,ylim=c(0,0.075),
     xlab=expression(paste("Body Mass Index (",Kg/m^2,")")),xlim=c(10,70),xaxt= "n",yaxt= "n")
axis(side=1,at=seq(10,70,10),labels=seq(10,70,10),pos=0)
axis(side=2,at=seq(0,0.075,0.025),labels=seq(0,0.075,0.025),pos=10,las=2)
hist(ped,breaks=20,col="blue",main="Histogram of Diabetes Pedigree Function",probability = T,ylim=c(0,2.5),
     xlab="Diabetes pedigree function ",xlim=c(0,2.6),xaxt= "n",yaxt= "n")
axis(side=1,at=seq(0,2.6,0.2),labels=seq(0,2.6,0.2),pos=0)
axis(side=2,at=seq(0,2.5,0.5),labels=seq(0,2.5,0.5),pos=0,las=2)
################################
#                              #
#       BLOCK IV               #
#       Transform Age          #
#                              #
################################
# Transform age
par(mfrow=c(2,4),mgp=c(1.5,0.5,0),mar=c(3.5,3,3.5,0.5))  # change margin at top of plots
hist(age,breaks=20,col="blue",main="Histogram of Age",xaxt= "n",yaxt= "n",probability = T,ylim=c(0,0.1),xlim=c(20,90))
axis(side=1,at=seq(20,90,5),labels=seq(20,90,5),pos=0)
axis(side=2,at=seq(0,0.1,0.02),labels=seq(0,0.1,0.02),pos=20,las=2)
hist(log(age),col="blue",main="Histogram of log Age",probability = T)
hist(1/sqrt(age),col="blue",main=expression(Histogram~of~frac(1,sqrt(Age))),probability = T)
hist(1/age,col="blue",main=expression(Histogram~of~frac(1,Age)),probability = T)
hist(1/age,breaks=seq(0.01,0.05,0.004),col="blue",main=expression(paste(Histogram~of~frac(1,Age)," ;  10 bars")),probability = T)
hist(1/age^2,col="blue",main=expression(Histogram~of~frac(1,Age^2)),probability = T)
hist(1/age^2,breaks=seq(0.00015, 0.0025,0.00025),col="blue",main=expression(paste(Histogram~of~frac(1,Age^2)," ;  9 bars")),probability = T)
hist(1/age^2,breaks=seq(0.00015, 0.0025,0.00025),col="blue",main=expression(paste(Histogram~of~frac(1,Age^2)," ;  9 bars")),probability = T,xlim=c(0,0.0025))
abline(v=mean(1/age^2),col="red")
abline(v=c(mean(1/age^2)-2*sd(1/age^2),mean(1/age^2+2*sd(1/age^2))),col="red",lty=2)
legend("bottomleft",legend=c("mean","+/- 2sd"),col=c("red"),lty=c(1,2),inset=c(0.1,0.1))
################################
#                              #
#       BLOCK V                #
#       Transform Diabetes     #
#       Pedigree Function      #
#                              #
################################
#Transform Pedigree function
par(mfrow=c(2,2),mgp=c(1.5,0.5,0),mar=c(3,3,2,1))  # change margin at top of plots & grid to 2x2
hist(ped,breaks=20,col="blue",main="Histogram of Diabetes Pedigree Function",probability = T,ylim=c(0,2.5),
     xlab="Diabetes pedigree function ",xlim=c(0,2.6),xaxt= "n",yaxt= "n")
axis(side=1,at=seq(0,2.6,0.2),labels=seq(0,2.6,0.2),pos=0)
axis(side=2,at=seq(0,2.5,0.5),labels=seq(0,2.5,0.5),pos=0,las=2)
hist(log(ped),col="blue",main="Histogram of log of Diabetes Pedigree Function",probability = T,
     xlab="Log of Diabetes pedigree function ",cex.main=1)
hist(log(ped),breaks=seq(-2.5,1.1,0.3),col="blue",main="Histogram of log of Diabetes Pedigree Function",probability = T,
     xlab="Log of Diabetes pedigree function ",cex.main=1)
hist(log(ped),breaks=seq(-3,1.1,0.3),col="blue",main="Histogram of log of Diabetes Pedigree Function",probability = T,
     xlab="Log of Diabetes pedigree function ",cex.main=1)
abline(v=mean(log(ped)),col="red")
abline(v=c(mean(log(ped))-2*sd(log(ped)),mean(log(ped))+2*sd(log(ped))),col="red",lty=2)
abline(v=c(mean(log(ped))-3*sd(log(ped)),mean(log(ped))+3*sd(log(ped))),col="red",lty=3)
legend("topleft",legend=c("mean","+/- 2sd","+/- 3sd"),col=c("red"),lty=c(1,2,3),inset=c(0.05,0.05))
################################
#                              #
#    BLOCK VI                  #
#    Box Plots to look         #
#    for unusual observations  #
#                              #
################################
par(mfrow=c(2,3),mgp=c(1.5,0.5,0),mar=c(3,3,2,1))  # change margin at top of plots & grid to 2x3
Boxplot(1/age^2,col="blue",main=" Reciprocal square Age",id.method="y")
Boxplot(glu,col="blue",main="Glucose Concentration (mg/dL)",id.method="y")
Boxplot(bp,col="blue",main="Diastolic Blood Pressure (mm Hg)",id.method="y")
Boxplot(skin,col="blue",main="Triceps Skin Thickness",id.method="y")
Boxplot(bmi,col="blue",main="Body mass Index",id.method="y")
Boxplot(log(ped),col="blue",main="Log of Diabetes Pedigree Function",id.method="y")


################################
#                              #
#       BLOCK VII              #
#       Plot Data: Box Plots   #
#                              #
################################
#Set some graphical parameters
# move axis labels and titles closer (mgp) and make margins smaller (mar)
par(mfrow=c(2,4),mgp=c(1.7,0.5,0),mar=c(3.5,3.5,0.5,0.5)) 
plot(type,col="blue", xlab="Diagnosed with Diabetes")
boxplot(npreg~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Number of Pregnacies", col="blue")
boxplot(glu~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Glucose Concentration", col="blue")
boxplot(bp~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Diastolic Blood Pressure (mm  Hg)", col="blue")
boxplot(bmi~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Body mass index (weight in kg/(height in m)^2)", col="blue")
boxplot(ped~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Diabetes pedigree function", col="blue")
boxplot(skin~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Triceps Skin Fold Thickness (mm)", col="blue")
boxplot(age~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Age", col="blue")
plot(type,col="blue", xlab="Diagnosed with Diabetes")
Boxplot(npreg~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Number of Pregnacies", col="blue")
Boxplot(glu~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Glucose Concentration", col="blue")
Boxplot(bp~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Diastolic Blood Pressure (mm  Hg)", col="blue")
Boxplot(bmi~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Body mass index (weight in kg/(height in m)^2)", col="blue")
Boxplot(ped~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Diabetes pedigree function", col="blue")
Boxplot(skin~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Triceps Skin Fold Thickness (mm)", col="blue")
Boxplot(age~type,
        xlab = "Diagnosed with Diabetes",
        ylab = "Age", col="blue")
################################
#                              #
#       BLOCK VIII             #
#       Plot Data              #
#                              #
################################
# Run ggpairsPIma file first

ggpairs(Pima,mapping=aes(color=type,alpha=0.4),
        lower = list(continuous= mod_points,combo=mod_bihist),
        diag=list(continuous=modified_density,discrete=mod_bar),
        upper=list(continuous=mod_cor,combo=mod_box))

################################
#                              #
#    BLOCK IX                  #
#    Empirical Logit function  #
#                              #
################################

myemplogit <- function(yvar=y,xvar=x,maxbins=10,sc=1,line=TRUE,...){
  breaks  <<- unique(quantile(xvar, probs=0:maxbins/maxbins))
  levs  <<- (cut(xvar, breaks, include.lowest=FALSE))
  num <<- as.numeric(levs)
  c.tab <- count(num,'levs')
  c.tab$levs <- factor(c.tab$levs, levels = levels(addNA(c.tab$levs)), labels = c(levels(c.tab$levs),
                                            paste("[",min(xvar),"]",sep="")), exclude = NULL)
  c.tab <- c.tab[c(nrow(c.tab),1:nrow(c.tab)-1),]
  sc <- (max(c.tab$freq)/min(c.tab$freq)/sc)^2
  zcex <<- sqrt(c.tab$freq/pi)/sc
  print(c.tab);print(zcex);print(sc)
  emplogitplot1(yvar~xvar,breaks=breaks,cex=zcex,showline=line,...)
}
################################
#                              #
#    BLOCK X                   #
#    Use the                   #
#    Empirical Logit function  #
#                              #
################################
par(mfrow=c(1,1),mgp=c(1.5,0.5,0),mar=c(3,3,1.2,0.5))  # change grid back to 1x1
# How to call the function
myemplogit(Pima$type,Pima$glu,30,sc=15,xlab="Glucose Concentration",line=FALSE)
# SINCE WE ATTACHED THE DATA WE CAN GET AWAY WITH THIS
myemplogit(type,npreg,20,sc=3,xlab="Number of Pregnacies",line=FALSE)
myemplogit(type,bp,20,sc=25,xlab="Diastolic Blood Pressure (mm  Hg)",line=FALSE)
myemplogit(type,skin,30,sc=10,xlab="Triceps Skin Fold Thickness (mm)",line=FALSE)
myemplogit(type,bmi,30,sc=6,xlab="Body mass index (weight in kg/(height in m)^2",line=FALSE)  
myemplogit(type,ped,30,sc=6,xlab="Diabetes pedigree function",line=FALSE)
myemplogit(type,age,30,sc=3,xlab="Age  ",line=FALSE)


################################
#                              #
#       BLOCK XI               #
#       Plot Data              #
#                              #
################################
par(mfrow=c(3,3),mgp=c(1.7,0.5,0),mar=c(3.5,3.5,0.5,0.5)) 
#Now re-run BLOCK X but NOT first line (not the par statement)
################################
#                              #
#       BLOCK XII              #
#       Try transformations    #
#                              #
################################
myemplogit(type,bmi,30,sc=8,xlab="Body mass index (weight in kg/(height in m)^2",line=FALSE)  
myemplogit(type,log(bmi),30,sc=8,xlab="Log of Body mass index (weight in kg/(height in m)^2",line=FALSE)  
myemplogit(type,1/bmi,30,sc=14,xlab="Reciprocal of Body mass index (weight in kg/(height in m)^2",line=FALSE)  
myemplogit(type,ped,30,sc=8,xlab="Diabetes pedigree function",line=FALSE)
myemplogit(type,log(ped),30,sc=8,xlab="Log of Diabetes pedigree function ",line=FALSE)
myemplogit(type,1/ped,30,sc=14,xlab="Reciprocal Diabetes pedigree function ",line=FALSE)
myemplogit(type,skin,30,sc=12,xlab="Triceps Skin Fold Thickness (mm)",line=FALSE)
myemplogit(type,log(skin),30,sc=12,xlab="Log of Triceps Skin Fold Thickness (mm)",line=FALSE)
myemplogit(type,1/skin,30,sc=18,xlab="Reciprocal of Triceps Skin Fold Thickness (mm)",line=FALSE)


################################
#                              #
#       BLOCK XIII             #
#       fit Model              #
#                              #
################################
fit0 <- glm(type~(1),family=binomial)
summary(fit0)
fit1 <- glm(type~npreg+glu+bp+bmi+ped+age+skin,family=binomial)
summary(fit1)
fit2 <- glm(type~npreg+glu+bp+I(1/bmi)+log(ped)+poly(age,2)+log(skin),family=binomial)
summary(fit2)
anova(fit0,fit2,test="Chisq")  # global test to see if all terms could be removed

Anova(fit2)  # Check significance with age treated as one term (Note this is also what we do when we have a factor)
#Refit model without log(skin)
fit3 <- glm(type~npreg+glu+bp+I(1/bmi)+log(ped)+poly(age,2),family=binomial)
summary(fit3)
Anova(fit3)
#bp least significant
#refit model without bp
fit4 <- glm(type~npreg+glu+I(1/bmi)+log(ped)+poly(age,2),family=binomial)
summary(fit4)
Anova(fit4)
#npreg notsignificant
#refit model without npreg
fit5 <- glm(type~glu+I(1/bmi)+log(ped)+poly(age,2),family=binomial,data=Pima)
summary(fit5)
Anova(fit5)
# All terms now significant
# Check we were OK to remove all these terms by comparing this model with the original
anova(fit5,fit2,test="Chisq")

################################
#                              #
#       BLOCK XIV              #
#       Residual plots         #
#                              #
################################
par(mfrow=c(1,1),mgp=c(1.7,0.5,0),mar=c(3.5,3.5,3,0.5)) 
hist(predict(fit5))
hist(rstudent(fit5))
#different residual plots, first against fitted values
arm::binnedplot(x=predict(fit5,type="response"),y=rstudent(fit5,type="pearson"),nclass=40,col.int=NA, main ="Binned Student Pearson Residuals") # student residual and 40 bins
arm::binnedplot(predict(fit5),rstandard(fit5,type="pearson"),nclass=40,col.int=NA, main ="Binned Standardised Pearson Residuals") # student residual and 40 bins
arm::binnedplot(predict(fit5),rstandard(fit5,type="deviance"),nclass=40,col.int=NA, main ="Binned Standardised Deviance Residuals") # student residual and 40 bins
par(mfrow=c(3,3),mgp=c(1.7,0.5,0),mar=c(3.5,3.5,3,0.5)) 
arm::binnedplot(x=bmi,y=rstudent(fit5),nclass=40,col.int=NA,xlab="Body mass index (weight in kg/(height in m)^2)", main ="Binned Student Deviance Residuals against bmi")
arm::binnedplot(x=ped,y=rstudent(fit5),nclass=40,col.int=NA,xlab="Diabetes pedigree function", main ="Binned Student Deviance Residuals against pedigree")
arm::binnedplot(x=age,y=rstudent(fit5),nclass=40,col.int=NA,xlab="Age", main ="Binned Student Deviance Residuals against age")
arm::binnedplot(x=glu,y=rstudent(fit5),nclass=40,col.int=NA,xlab="Glucose Concentration", main ="Binned Student Deviance Residuals against glucose")
arm::binnedplot(x=skin,y=rstudent(fit5),nclass=40,col.int=NA,xlab="Triceps Skin Fold Thickness (mm)", main ="Binned Student Deviance Residuals against skinthickness")
arm::binnedplot(x=bp,y=rstudent(fit5),nclass=40,col.int=NA,xlab="Diastolic Blood Pressure (mm  Hg)", main ="Binned Student Deviance Residuals against bp")
arm::binnedplot(x=npreg,y=rstudent(fit5),nclass=40,col.int=NA,xlab="Diastolic Blood Pressure (mm  Hg)", main ="Binned Student Deviance Residuals against npreg")
arm::binnedplot(x=predict(fit5),y=rstandard(fit5,type="deviance"),nclass=40,col.int=NA) 
arm::binnedplot(predict(fit5),rstandard(fit5,type="pearson"),nclass=40,col.int=NA) # student residual and 40 bins
################################
#                              #
#       BLOCK XV               #
#       Residual plots         #
#                              #
################################
par(mfrow=c(1,1),mgp=c(1.7,0.5,0),mar=c(3.5,3.5,3,0.5)) 

hnp(fit5,resid.type="deviance",ylab="Deviance Residuals")
hnp(fit5,resid.type="pearson", ylab="Pearson Residuals")
################################
#                              #
#       BLOCK XVI              #
#       Diagnostic plots       #
#                              #
################################
#Added Varaible Plots examples
avPlots(fit5)
avPlots(fit2)
#Partial Residual Plots
crPlots(fit2,id=TRUE)
crPlots(fit5,id=TRUE)
#dfbetas
dfbetaPlots(fit5,intercept = TRUE)
#Standardised dfbetas
dfbetasPlots(fit5,intercept = TRUE,id.n=3)
#dffits
dffitsPlots(fit5)
dfs <- dffits(fit5)
plot(dfs~c(1:length(Pima$type)),xlab="Index",main="DFfits")
##############  WARNING  WARNING ##################
######  Next part is interactive!!!  ##############
######  Run this line and then click on points in graphics window   ######
######  that you want to identify  #######################################
###### Click on "Finish" in top right of plot when finished   ############
##########################################################################
identify(c(1:length(Pima$type)),dfs)
influenceIndexPlot(fit5)  #  Click on any points you wish to identify.
# Default residual Plots
par(mfrow=c(2,3)) # set grid for plots
plot(fit5,which=1:6)
par(mfrow=c(1,1)) # set grid back to 1x1

#check for multicollinearity

car::vif(fit5)  # ensure to use version from car package
################################
#                              #
#       BLOCK XVII             #
#       Understand Model       #
#                              #
################################
plot(allEffects(fit5))  # effect plots

# Refit model so as to display plot of standardised odds ratios
Pima$lped <- log(Pima$ped)
Pima$rbmi <- 1/Pima$bmi
a <- poly(Pima$age,2)
Pima$a1 <- a[,1]
Pima$a2 <- a[,2]
fit5a <- glm(type~glu+rbmi+lped+a1+a2,data=Pima,family=binomial)

summary(fit5a)

plot_model(fit5a,type="std",axis.lim = c(0.4,2))
tab_model(fit5a,show.est=TRUE,show.std=TRUE)
tab_model(fit5a,show.est=TRUE,show.std=TRUE,transform=NULL)

################################
#                              #
#       BLOCK XVIII            #
#       Model Performance      #
#                              #
################################

## Classification
predicted <- predict(fit5, type='response')
boxplot(predicted~type, col="blue")

# Confusion matrix
ypred <- predicted > 0.5
addmargins(table(type, ypred))

##Accuracy
(314+ 110)/532

# Cross-validated prediction accuracy
fit1 <- glm(type~npreg+glu+bp+bmi+ped+age+skin,family=binomial,data=Pima)
fit2 <- glm(type~npreg+glu+bp+I(1/bmi)+log(ped)+poly(age,2)+log(skin),family=binomial,data=Pima)
fit5 <- glm(type~glu+I(1/bmi)+log(ped)+poly(age,2),family=binomial,data=Pima)
CVbinary(fit1)
CVbinary(fit2)
CVbinary(fit5)

# False positive and negative rates
FPR <- 41/(314+41)
FNR <-67/(67+110)
FPR
FNR

#Basic ROC curve

pred <- prediction(predicted,type)
perf <- performance(pred,"fnr","fpr")
plot(perf)
# Traditional ROC curve
perf2 <- performance(pred,"tpr","tnr")
plot(perf2,xlim=c(1,0))

#Alternative ROC

roc1 <- roc(type,predict(fit5, type='response'))
plot(roc1,xlim=c(1,0))
auc(roc1)  # Calculate AUC
auc(type,predict(fit5, type='response')) # calculate the AUC from the model
ci(roc1)  # confidence interval for AUC

plot.roc(type,predict(fit5, type='response'),ci=TRUE,of="thresholds",ci.type="shape",print.auc=TRUE)

rocte <- roc(Pima.te$type, predict(fit5,type="response",data.frame=Pima.te))
predict()

#  Re-do with seperate training and test data
fit5A <- glm(type~glu+I(1/bmi)+log(ped)+poly(age,2),family=binomial,data=Pima.tr) # fit model o=using only training data
pit=stats::predict(fit5A,new=Pima.te) # obtain fitted values for test data using model fitted to training (FYI only)
plot.roc(Pima.tr$type,predict(fit5A, type='response'),ci=TRUE,of="thresholds",ci.type="shape",print.auc=TRUE,col="red",main="Training ROC")  # plot ROC for training
plot.roc(Pima.te$type,predict(fit5A,type="response", new=Pima.te),ci=TRUE,of="thresholds",ci.type="shape",print.auc=TRUE,main ="Test ROC")
#Now both together
plot.roc(Pima.te$type,predict(fit5A,type="response", new=Pima.te),ci=TRUE,of="thresholds",ci.type="shape",print.auc=TRUE, main=" Both ROC")
#add = TRUE will add over the top of the previous plot.  We change the colour and location of the printed AUC so we know which is which.
plot.roc(Pima.tr$type,predict(fit5A, type='response'),add=TRUE,col="red",print.auc=TRUE, print.auc.x= 0.5, print.auc.y=0.3,print.auc.col="red")
legend("bottomright",legend=c("Training","Test"),col=c("red","black"),lty=c(1,1),inset=c(0.05,0.05)) # Add a legend

# Compare Models
plot.roc(Pima$type,predict(fit1,type="response"),ci=TRUE,of="thresholds",ci.type="shape",print.auc=TRUE, main=" Compare ROC", print.auc.x= 0.6, print.auc.y=0.75)
plot.roc(Pima$type,predict(fit2, type='response'),add=TRUE,col="red",print.auc=TRUE, print.auc.x= 0.6, print.auc.y=0.6,print.auc.col="red") # again add=TRUE adds to previous plot
plot.roc(Pima$type,predict(fit5, type='response'),add=TRUE,col="blue",print.auc=TRUE, print.auc.x= 0.6, print.auc.y=0.25,print.auc.col="blue") # add=TRUE adds to previous plots
legend("bottomright",legend=c("Full no transform","Full with transform","Reduced with transformed"),col=c("black","red","blue"),lty=c(1,1,1),inset=c(0.05,0.05)) # legend
################################
#                              #
#       BLOCK XIX              #
# Plotting an ordinal variable #
#                              #
################################
# Strange histograms of npreg!
hist(npreg,breaks=seq(-0.5,18,1.5),col="blue",xaxt="n")
axis(side=1,at=seq(-0.5,18,1.5),labels=seq(-0.5,18,1.5),pos=0)
hist(npreg,breaks=seq(-1,18,1.5),col="blue",xaxt="n")
axis(side=1,at=seq(-1,18,1.5),labels=seq(-1,18,1.5),pos=0)
