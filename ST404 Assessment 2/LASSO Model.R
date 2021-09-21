load("USACrime.rda")

#Library
library(car)
library(dplyr)
library(glmnet)
library(ggplot2)


#Data Cleaning
USACrime$ownHousMed[USACrime$ownHousMed==500001]<-NA 
USACrime$rentMed[USACrime$rentMed==1001]<-NA 
USACrime$ownHousQrange[USACrime$ownHousQrange==0]<-NA 
USACrime$rentQrange[USACrime$rentQrange==0]<-NA 

USACrime <- na.omit(USACrime)

USACrime$State<-droplevels(USACrime$State)
levels(USACrime$region) <- c("Midwest", "NorthEast", "West", "South", "West") 

USACrimeCopy <- USACrime

USACrime = subset(USACrime, select = -c(State,region))
      

#Transformation
USACrime$violentPerPop <- log(USACrime$violentPerPop,2)
USACrime$nonViolPerPop <- log(USACrime$nonViolPerPop,2)

#Categorical Transformation
USACrime = USACrime %>% mutate(pctUrban = ifelse(pctUrban >= 85,1,0))

#Log2 Transformations
USACrime$medIncome <- log(USACrime$medIncome,2)
USACrime$pctLowEdu <- log(USACrime$pctLowEdu,2)
USACrime$pctUnemploy <- log(USACrime$pctUnemploy,2)

#Log2+1 Transformations
USACrime$pctKidsBornNevrMarr <- log(USACrime$pctKidsBornNevrMarr+1,2)
USACrime$pctVacantBoarded <- log(USACrime$pctVacantBoarded+1,2)

#Power Transformations
USACrime$pctEmploy <- (USACrime$pctEmploy)^2
USACrime$pctHousOccup <- (USACrime$pctHousOccup)^3

#Root Transformations
USACrime$pctVacant6up <- sqrt(USACrime$pctVacant6up)
USACrime$popDensity <- sqrt(USACrime$popDensity)







#Variable Selection(VIF)
model1 <- lm(violentPerPop ~ pctUrban + medIncome + pctWdiv + pctLowEdu + pctNotHSgrad + 
               pctCollGrad + pctUnemploy + pctEmploy +pctKids2Par + pctKidsBornNevrMarr + pctHousOccup +
               pctHousOwnerOccup + pctVacantBoarded + pctVacant6up + ownHousMed + ownHousQrange + rentMed +
               rentQrange + popDensity + pctForeignBorn, data = USACrime)
vif(model1)

model1 <- lm(violentPerPop ~ pctUrban + medIncome + pctLowEdu + pctUnemploy + pctEmploy + pctKidsBornNevrMarr + 
               pctHousOccup + pctHousOwnerOccup + pctVacantBoarded + pctVacant6up + popDensity + 
               pctForeignBorn, data = USACrime)
vif(model1)





#LASSO Regression (violentPerPop)
USACrimeLasso.viol = subset(USACrime, select = c(violentPerPop, pctUrban, medIncome, pctLowEdu, pctUnemploy,
                                                 pctEmploy, pctKidsBornNevrMarr, pctHousOccup, pctHousOwnerOccup, 
                                                 pctVacantBoarded, pctVacant6up, popDensity, pctForeignBorn))

exp.variables <- data.matrix(USACrimeLasso.viol[,2:13])
out.variables <- USACrimeLasso.viol$violentPerPop

#Fit and testing the lasso model
lassoviol.fit <- cv.glmnet(exp.variables, out.variables, type.measure = "mse", alpha = 1, family = "gaussian")
lassoviol.predicted <- predict(lassoviol.fit, s = lassoviol.fit$lambda.min, newx = exp.variables)

plot(lassoviol.fit, main = "10-fold Cross-Validation for optimal Lambda") #Graph for Lambda

mean((out.variables -lassoviol.predicted)^2) #Finding Mean-Squared Error

#Look at the coefficients
coef(lassoviol.fit)

#Graph Plotting
lassoviol.predict.test <- data.frame(lassoviol.predicted, out.variables)
lassoviol.predict.test$State <- USACrimeCopy$State #Add State and region variables back for analysis
lassoviol.predict.test$region <- USACrimeCopy$region

ggplot(data = lassoviol.predict.test, aes(X1, out.variables)) +
  geom_point(aes(color = region))

#Residual Plot
ggplot(data = lassoviol.predict.test , aes(X1,(X1-out.variables))) +
  geom_point(aes(color = region))

ggplot(data = lassoviol.predict.test , aes(X1,(X1-out.variables))) +
  geom_point(aes(color = State))

#South
ggplot(data = lassoviol.predict.test[which(lassoviol.predict.test$region=="South"),] , aes(X1,(X1-out.variables))) +
  geom_point(color = "red")

#RI
ggplot(data = lassoviol.predict.test[which(lassoviol.predict.test$State=="RI"),] , aes(X1,(X1-out.variables))) +
  geom_point(color = "orange")

#PA
ggplot(data = lassoviol.predict.test[which(lassoviol.predict.test$State=="PA"),] , aes(X1,(X1-out.variables))) +
  geom_point(color = "blue")

#Looking at Residual>3
ggplot(data = lassoviol.predict.test[which(abs(lassoviol.predict.test$X1-lassoviol.predict.test$out.variables) > 3),] , aes(X1,(X1-out.variables))) +
  geom_point(aes(color = State)) +
  geom_text(aes(label=State),hjust=0, vjust=0)

#Looking in the prediction of places that have extremely high violent crimes
ggplot(data = lassoviol.predict.test[which(lassoviol.predict.test$out.variables > 11.5),] , aes(X1,(X1-out.variables))) +
  geom_point(aes(color = region))

#QQplot
lassoviol.predict.test$residual <- (lassoviol.predict.test$X1 - lassoviol.predict.test$out.variables)
qqnorm(lassoviol.predict.test$residual, main = "Q-Q plot for violent model residuals")






#Variable Selection(VIF)
model2 <- lm(nonViolPerPop ~ pctUrban + medIncome + pctWdiv + pctLowEdu + pctNotHSgrad + 
               pctCollGrad + pctUnemploy + pctEmploy +pctKids2Par + pctKidsBornNevrMarr + pctHousOccup +
               pctHousOwnerOccup + pctVacantBoarded + pctVacant6up + ownHousMed + ownHousQrange + rentMed +
               rentQrange + popDensity + pctForeignBorn, data = USACrime)
vif(model2)

model2 <- lm(nonViolPerPop ~ pctUrban + medIncome + pctLowEdu +
               pctUnemploy + pctEmploy + pctKidsBornNevrMarr + pctHousOccup +
               pctHousOwnerOccup + pctVacantBoarded + pctVacant6up +
               popDensity + pctForeignBorn, data = USACrime)
vif(model2)


#LASSO Regression (nonViolPerPop)
USACrimeLasso.nonviol = subset(USACrime, select = c(nonViolPerPop, pctUrban, medIncome, pctLowEdu, pctUnemploy,
                                                 pctEmploy, pctKidsBornNevrMarr, pctHousOccup, pctHousOwnerOccup, 
                                                 pctVacantBoarded, pctVacant6up, popDensity, pctForeignBorn))

exp.variables <- data.matrix(USACrimeLasso.nonviol[,2:13])
out.variables <- USACrimeLasso.nonviol$nonViolPerPop

#Fit and testing the lasso model
lassononviol.fit <- cv.glmnet(exp.variables, out.variables, type.measure = "mse", alpha = 1, family = "gaussian")
lassononviol.predicted <- predict(lassononviol.fit, s = lassononviol.fit$lambda.1se, newx = exp.variables)

plot(lassononviol.fit, main = "10-fold Cross-Validation for optimal Lambda") #Graph for Lambda

mean((out.variables - lassononviol.predicted)^2) #Finding Mean-Squared Error

#Look at the coefficients
coef(lassononviol.fit)

#Graph Plotting
lassononviol.predict.test <- data.frame(lassononviol.predicted, out.variables)
lassononviol.predict.test$State <- USACrimeCopy$State #Add State and region variables back for analysis
lassononviol.predict.test$region <- USACrimeCopy$region

ggplot(data = lassononviol.predict.test, aes(X1, out.variables)) +
  geom_point(aes(color = region))

#Residual Plot
ggplot(data = lassononviol.predict.test , aes(X1,(X1-out.variables))) +
  geom_point(aes(color = region))

ggplot(data = lassononviol.predict.test , aes(X1,(X1-out.variables))) +
  geom_point(aes(color = State))

#NorthEast
ggplot(data = lassononviol.predict.test[which(lassononviol.predict.test$region=="NorthEast"),] , aes(X1,(X1-out.variables))) +
  geom_point(color = "red")

#MA
ggplot(data = lassononviol.predict.test[which(lassononviol.predict.test$State=="MA"),] , aes(X1,(X1-out.variables))) +
  geom_point(color = "orange")

#LA
ggplot(data = lassononviol.predict.test[which(lassononviol.predict.test$State=="LA"),] , aes(X1,(X1-out.variables))) +
  geom_point(color = "blue")

#Looking at Residual>2
ggplot(data = lassononviol.predict.test[which(abs(lassononviol.predict.test$X1-lassononviol.predict.test$out.variables) > 2),] , aes(X1,(X1-out.variables))) +
  geom_point(aes(color = State)) +
  geom_text(aes(label=State),hjust=0, vjust=0)

#Looking in the prediction of places that have extremely high non-violent crimes
ggplot(data = lassononviol.predict.test[which(lassononviol.predict.test$out.variables > 13.7),] , aes(X1,(X1-out.variables))) +
  geom_point(aes(color = region))

#QQplot
lassononviol.predict.test$residual <- (lassononviol.predict.test$X1 - lassononviol.predict.test$out.variables)
qqnorm(lassononviol.predict.test$residual, main = "Q-Q plot for nonviolentmodel residuals")
