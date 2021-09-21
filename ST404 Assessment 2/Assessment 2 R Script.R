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

USACrime = subset(USACrime, select = -c(State,region) )


#Transformation
USACrime$violentPerPop <- log(USACrime$violentPerPop,2)
USACrime$nonViolPerPop <- log(USACrime$nonViolPerPop,2)


#Variable Selection(VIF)
model1 <- lm(violentPerPop ~ pctUrban + medIncome + pctWdiv + pctLowEdu + pctNotHSgrad + 
               pctCollGrad + pctUnemploy + pctEmploy +pctKids2Par + pctKidsBornNevrMarr + pctHousOccup +
               pctHousOwnerOccup + pctVacantBoarded + pctVacant6up + ownHousMed + ownHousQrange + rentMed +
               rentQrange + popDensity + pctForeignBorn, data = USACrime)
vif(model1)

model1 <- lm(violentPerPop ~ pctUrban + medIncome + pctLowEdu +
               pctUnemploy + pctEmploy + pctKidsBornNevrMarr + pctHousOccup +
               pctHousOwnerOccup + pctVacantBoarded + pctVacant6up +
               popDensity + pctForeignBorn, data = USACrime)
vif(model1)

#AIC
#ViolentPerPop
model0 <- lm(violentPerPop ~ 1, data = USACrime)

step(model0, direction="both", 
     scope=list("lower"=model0, "upper"=model1))

#Removed pctHousOwnerOccup and pctLowEdu
#lm(formula = violentPerPop ~ pctKidsBornNevrMarr + medIncome + 
#     pctForeignBorn + pctHousOccup + pctVacant6up + pctUrban + 
#     pctVacantBoarded + popDensity + pctEmploy + pctUnemploy, 
#   data = USACrime)


#nonViolPerPop
model1 <- lm(nonViolPerPop ~ pctUrban + medIncome + pctLowEdu +
               pctUnemploy + pctEmploy + pctKidsBornNevrMarr + pctHousOccup +
               pctHousOwnerOccup + pctVacantBoarded + pctVacant6up +
               popDensity + pctForeignBorn, data = USACrime)

model0 <- lm(nonViolPerPop ~ 1, data = USACrime)

step(model0, direction="both", 
     scope=list("lower"=model0, "upper"=model1))

#Removed pctEmploy and pctUnemploy
#lm(formula = nonViolPerPop ~ medIncome + pctKidsBornNevrMarr + 
#     pctVacant6up + pctHousOccup + pctUrban + pctHousOwnerOccup + 
#     pctVacantBoarded + popDensity + pctForeignBorn + pctLowEdu, 
#   data = USACrime)





#Ridge Regression (violentPerPop)
USACrimeRidge.viol = subset(USACrime, select = c(violentPerPop, pctKidsBornNevrMarr, medIncome, pctForeignBorn, 
                                       pctHousOccup, pctVacant6up, pctUrban, pctVacantBoarded, popDensity, 
                                       pctEmploy,pctUnemploy))

#Creating training and testing data set (70% training and 30% testing)
set.seed(1) 

index = sample(1:nrow(USACrimeRidge.viol), 0.7*nrow(USACrimeRidge.viol)) 

train = USACrimeRidge.viol[index,] # Create the training data 
test = USACrimeRidge.viol[-index,] # Create the test data

x.train <- subset(train, select = -c(violentPerPop))
x.train <- matrix(unlist(x.train), ncol = 10)
y.train <- subset(train, select = c(violentPerPop))
y.train <- unlist(y.train)

x.test <- subset(test, select = -c(violentPerPop))
x.test <- matrix(unlist(x.test), ncol = 10)
y.test <- subset(test, select = c(violentPerPop))
y.test <- unlist(y.test)

#Fit and testing the ridge model
ridgeviol.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 0, family = "gaussian")
ridgeviol.predicted <- predict(ridgeviol.fit, s = ridgeviol.fit$lambda.min, newx = x.test)

mean((y.test - ridgeviol.predicted)^2)



#Ridge Regression (nonViolPerPop)
USACrimeRidge.nonviol = subset(USACrime, select = c(nonViolPerPop, pctKidsBornNevrMarr, medIncome, pctForeignBorn, 
                                                 pctHousOccup, pctVacant6up, pctUrban, pctVacantBoarded, popDensity, 
                                                 pctEmploy,pctUnemploy))

#Creating training and testing data set (70% training and 30% testing)
set.seed(1) 

index = sample(1:nrow(USACrimeRidge.nonviol), 0.7*nrow(USACrimeRidge.nonviol)) 

train = USACrimeRidge.nonviol[index,] # Create the training data 
test = USACrimeRidge.nonviol[-index,] # Create the test data

x.train <- subset(train, select = -c(nonViolPerPop))
x.train <- matrix(unlist(x.train), ncol = 10)
y.train <- subset(train, select = c(nonViolPerPop))
y.train <- unlist(y.train)

x.test <- subset(test, select = -c(nonViolPerPop))
x.test <- matrix(unlist(x.test), ncol = 10)
y.test <- subset(test, select = c(nonViolPerPop))
y.test <- unlist(y.test)

#Fit and testing the ridge model
ridgenonviol.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 0, family = "gaussian")
ridgenonviol.predicted <- predict(ridgenonviol.fit, s = ridgenonviol.fit$lambda.min, newx = x.test)

mean((y.test - ridgenonviol.predicted)^2)








#LASSO Regression (violentPerPop)
USACrimeLasso.viol = subset(USACrime, select = c(violentPerPop, pctUrban, medIncome, pctLowEdu, pctUnemploy,
                                                 pctEmploy, pctKidsBornNevrMarr, pctHousOccup, pctHousOwnerOccup, 
                                                 pctVacantBoarded, pctVacant6up, popDensity, pctForeignBorn))


#Creating training and testing data set (70% training and 30% testing)
set.seed(1) 

index = sample(1:nrow(USACrimeLasso.viol), 0.7*nrow(USACrimeLasso.viol)) 

train = USACrimeLasso.viol[index,] # Create the training data 
test = USACrimeLasso.viol[-index,] # Create the test data

x.train <- data.matrix(train[,2:13])
y.train <- train$violentPerPop

x.test <- data.matrix(test[,2:13])
y.test <- test$violentPerPop

#Fit and testing the lasso model
lassoviol.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 1, family = "gaussian")
lassoviol.predicted <- predict(lassoviol.fit, s = lassoviol.fit$lambda.1se, newx = x.test)

plot(lassoviol.fit, main = "10-fold Cross-Validation for optimal Lambda")

mean((y.test -lassoviol.predicted)^2)

#Look at the coefficients
lassoviol_coef = predict(lassoviol.fit, type = "coefficients", s = lassoviol.fit$lambda.1se)[1:13,]
lassoviol_coef[lassoviol_coef != 0]
coef(lassoviol.fit)

#Residual Plot
lassoviol.predict.test <- data.frame(lassoviol.predicted, y.test)
ggplot(data = lassoviol.predict.test , aes(X1,(X1-y.test))) +
  geom_point()








#LASSO Regression (nonViolPerPop)
USACrimeLasso.viol = subset(USACrime, select = c(nonViolPerPop, pctUrban, medIncome, pctLowEdu, pctUnemploy,
                                                 pctEmploy, pctKidsBornNevrMarr, pctHousOccup, pctHousOwnerOccup, 
                                                 pctVacantBoarded, pctVacant6up, popDensity, pctForeignBorn))

#Creating training and testing data set (70% training and 30% testing)
set.seed(1) 

index = sample(1:nrow(USACrimeLasso.nonviol), 0.7*nrow(USACrimeLasso.nonviol)) 

train = USACrimeLasso.nonviol[index,] # Create the training data 
test = USACrimeLasso.nonviol[-index,] # Create the test data

x.train <- data.matrix(train[,2:13])
y.train <- train$nonViolPerPop

x.test <- data.matrix(test[,2:13])
y.test <- test$nonViolPerPop

#Fit and testing the lasso model
lassononviol.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha = 1, family = "gaussian")
lassononviol.predicted <- predict(lassononviol.fit, s = lassononviol.fit$lambda.1se, newx = x.test)

plot(lassononviol.fit, main = "10-fold Cross-Validation for optimal Lambda")

mean((y.test -lassononviol.predicted)^2)

#Look at the coefficients
lasso_nonviolcoef = predict(lassononviol.fit, type = "coefficients", s = lassononviol.fit$lambda.1se)[1:13,]
lasso_nonviolcoef[lasso_nonviolcoef != 0]
coef(lassononviol.fit)

#Residual Plot
lassononviol.predict.test <- data.frame(lassononviol.predicted, y.test)
ggplot(data = lassononviol.predict.test , aes(X1,(X1-y.test))) +
  geom_point()



