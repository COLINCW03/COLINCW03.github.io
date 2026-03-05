#Decsion Trees

Titanic_data <- read.csv("C:/Users/Colin/Downloads/Titanic.csv")
View(Titanic)
str(Titanic_data)
install.packages("rpart")
library(rpart)

Titanic_tree = rpart(Survived ~ Age + Sex + Class,method = "class",data = Titanic_data)


install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(Titanic_tree)

Tree_predictions = predict(Titanic_tree,data = Titanic_data, type = "class")

install.packages("gmodels")
library(gmodels)
CrossTable(Tree_predictions,Titanic_data$Survived)

accuracy = (270 + 1470)/2201
accuracy
missclass_rate = (20+ 441)/2201
missclass_rate

#sensitivity TP/(TP+FN) how good are we at predicting yes
sensitivity= 270/711
sensitivity
#specificity = TN/(TN+FP)
Specificty= 1470/1490
Specificty



# 3/10/2025 

mowers_data <- read.csv("C:/Users/Colin/Downloads/mowers.csv")
View(mowers_data)
str(mowers_data)
library(rpart)
library(rpart.plot)

mowers_tree = rpart(ownership~ ., data= mowers_data, method = "class")
mowers_tree 
rpart.plot(mowers_tree)
library(gmodels)
mowers_predictions = predict(mowers_tree,data = mowers_data, type = "class")
mowers_predictions

CrossTable(mowers_predictions,mowers_data$ownership)

#3/12/2025

bank_data <- read.csv("C:/Users/Colin/Downloads/bank_data.csv")
View(bank_data)

str(bank_data)

#Premilairy analysis
loan_counts = table(bank_data$personal_loan)
loan_counts

prop.table(loan_counts)

library(gmodels)

CrossTable(bank_data$education,bank_data$personal_loan)
prop.table(table(bank_data$education, bank_data$personal_loan))

CrossTable(bank_data$online_banking,bank_data$personal_loan)

set.seed(100)

#generate a random number to be used to select out datasets. 
n = nrow(bank_data)
random_sample = sample(5000, 4000)
n
random_sample = sample(n,.8*n)
random_sample

#calculate our training set. 

Training_set = bank_data[random_sample,]

#Create a model using the training set. 
library(rpart)
library(rpart.plot)

bank_model = rpart(personal_loan ~., data = Training_set, method = "class")
rpart.plot(bank_model)

#predict results of model. 
Training_set_predictions= predict(bank_model, data = Training_set, type = "class")

library(gmodels)
# comparing training set actuals  and predictions. 
CrossTable(Training_set_predictions, Training_set$personal_loan)

train_accuracy =  (349 + 3639)/ 4000
train_accuracy

train_misclass = (10 + 46)/ 4000
train_misclass

train_sensitivity = 349/395
train_sensitivity

train_specificity = 3595/3605
train_specificity 
# EXam 1 

SportingGoods_data <- read.csv("C:/Users/Colin/Downloads/SportingGoods.csv")
purchase_counts = table(SportingGoods_data$Purchase)
purchase_counts
prop.table(table(SportingGoods_data$Purchase))

age_purchase_counts = table(SportingGoods_data$AgeGroup, SportingGoods_data$Purchase)
age_purchase_counts
146+47
CrossTable(SportingGoods_data$FeedbackScore,SportingGoods_data$ExperienceType)
CrossTable(SportingGoods_data$Purchase,SportingGoods_data$ExperienceType)
#Homework 4
marketing_data<- read.csv("C:/Users/Colin/Downloads/marketing_data (1).csv")
count_table = table(marketing_data$Offer_Accepted)
count_table
16617+1383
1383/18000
16617/18000
library(rpart)
library(rpart.plot)
library(gmodels)
marketing_tree = rpart(Offer_Accepted~ ., data= marketing_data, method = "class")
rpart.plot(marketing_tree)

marketing_predictions=predict(marketing_tree, data = marketing_data, type = "class")
marketing_predictions
CrossTable(marketing_predictions,marketing_data$Offer_Accepted)

Marketing_accuracy= (481+16349)/18000
Marketing_accuracy
Marketing_misclass= (268+902)/18000
Marketing_misclass

#Homework #5
set.seed(250)
random_sample = sample(18000,14400)
Training_set <- marketing_data[random_sample,]
validation_set <- marketing_data[-random_sample,]
marketing_model = rpart(Offer_Accepted~ ., data = Training_set, method= "class")
rpart.plot(marketing_model)

Training_set_predictions= predict(marketing_model, data = Training_set, type = "class")
CrossTable(Training_set_predictions, Training_set$Offer_Accepted)

train_accuracy=(13026+433)/14400
train_accuracy

train_misclass= (223+718)/14400
train_misclass

train_sensitivity= 433/1151
train_sensitivity

train_specificity=(13026/13249)
train_specificity
(268+902)/18000
validation_set_predictions= predict(marketing_model, newdata = validation_set, type = "class")
CrossTable(validation_set_predictions, validation_set$Offer_Accepted)

validation_accuracy=(3305+93)/3600
validation_accuracy

validation_misclass= (139+63)/3600
validation_misclass

validation_sensitivity= (93/232)
validation_sensitivity

validation_specificity=(3305/3368)
validation_specificity

#3/21/25 Regression trees. 
corolla_data <- read.csv("C:/Users/Colin/Downloads/corolla.csv")
str(corolla_data)
#predictors 
#age,mileage,horsepower, engine_vol, num_doors
#catagorical predictors. 
#fuel_type,metalic, automatic. 

library(rpart)
library(rpart.plot)
library(gmodels)
options(scipen=999)
corolla_model <- rpart(price ~ .,data = corolla_data , method = "anova")
rpart.plot(corolla_model)
#3/24/2025
jbhunt_data <- read.csv("C:/Users/Colin/Downloads/jbhunt_data.csv")
str(jbhunt_data)
plot(jbhunt_data$Travel_Time ~ jbhunt_data$Miles_Traveled, xlab = "Miles Traveled", 
     ylab= "Travel Time")
cor(jbhunt_data$Miles_Traveled,jbhunt_data$Travel_Time)
#Single linear regression model. 
jbhunt_model= lm(Travel_Time ~ Miles_Traveled, data = jbhunt_data)
jbhunt_model
# Linear regression equation 
summary(jbhunt_model)


#Corolla regression model and summary

plot(corolla_data$mileage, corolla_data$price, main= "price vs mileage", ylab= "Price")
summary(Corolla_regression_model)
cor(corolla_data$price, corolla_data$mileage)
Corolla_regression_model= lm(price~ mileage, data= corolla_data)
Corolla_regression_model

#setting of notation. 
summary(jbhunt_model)

abline(jbhunt_model,col = "red")

corolla_data <- read.csv("C:/Users/108687/Desktop/BUSN305/BUSN305 Files/Lecture Notes/Week 9/corolla.csv")

str(corolla_data)

cor(corolla_data$mileage,corolla_data$price)

corolla_slr_model = lm(price ~ mileage,data = corolla_data)
summary(corolla_slr_model)
#Regression equation
# price = 15816.83915 - 0.06313 * mileage

#Getting rid of e notation
options(scipen = 999)

plot(corolla_data$price ~ corolla_data$mileage,xlab = "mileage", ylab = "Price")
abline(corolla_slr_model,col="red")
#Check for heterosecasitiy. 
plot(x=corolla_data$mileage, y= residuals)
abline