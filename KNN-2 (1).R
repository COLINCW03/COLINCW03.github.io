mowers_data <- read.csv("C:/Users/108687/Desktop/BUSN305/BUSN305 Files/Lecture Notes/Week 7/mowers.csv")
str(mowers_data)
#Target - ownership (categorical)
#Numerical Predictors: income, lot_size

#Plot income vs lotsize - add in the 3rd variable ownership
plot(x = mowers_data$income,y = mowers_data$lot_size,
     col = ifelse(mowers_data$ownership == '1',"blue","orange"))

#find the distance between our person interest (65000, 19) to (51000,22)
d = sqrt((65000 - 51000)^2 + (19 - 22)^2)
d

min_income = min(mowers_data$income)
min_income
max_income = max(mowers_data$income)
max_income
(65-min_income)/(max_income - min_income)
min_lotsize = min(mowers_data$lot_size)
min_lotsize
max_lotsize = max(mowers_data$lot_size)
max_lotsize
(19 - min_lotsize)/(max_lotsize - min_lotsize)

#normalize all numerical variables in our dataset
mowers_data$norm_income = (mowers_data$income - min_income)/(max_income - min_income)
mowers_data$norm_lotsize = (mowers_data$lot_size - min_lotsize)/(max_lotsize - min_lotsize)
plot(x = mowers_data$norm_income,y = mowers_data$norm_lotsize,
     col = ifelse(mowers_data$ownership == '1',"blue","orange"))

#~~~~
bank_data <- read.csv("C:/Users/108687/Desktop/BUSN305/BUSN305 Files/Lecture Notes/Week 7/bank_data.csv")
str(bank_data)
#Target: personal_loan
#Predictors:
#Numerical
#  age, experience_years, income, family_size, cc_avg_bal, mortgage_bal
#Categorical - Yes/No
#  securities_acct, cd_acct, online_banking, credit_card
#Categorical - Enumerated
#  education

#Step 1: Normalize our numerical predictors
#  age, experience_years, income, family_size, cc_avg_bal, mortgage_bal
bank_data$age_norm = (bank_data$age - min(bank_data$age))/
  (max(bank_data$age) - min(bank_data$age))

bank_data$expyears_norm = (bank_data$experience_years - min(bank_data$experience_years))/
  (max(bank_data$experience_years) - min(bank_data$experience_years))

bank_data$income_norm = (bank_data$income - min(bank_data$income))/
  (max(bank_data$income) - min(bank_data$income))

bank_data$famsize_norm = (bank_data$family_size - min(bank_data$family_size))/
  (max(bank_data$family_size) - min(bank_data$family_size))

bank_data$cc_bal_norm = (bank_data$cc_avg_bal - min(bank_data$cc_avg_bal))/
  (max(bank_data$cc_avg_bal) - min(bank_data$cc_avg_bal))

bank_data$mortgage_bal_norm = (bank_data$mortgage_bal - min(bank_data$mortgage_bal))/
  (max(bank_data$mortgage_bal) - min(bank_data$mortgage_bal))

#Step 2: Create dummy variables for all categorical predictors
#  securities_acct, cd_acct, online_banking, credit_card
bank_data$sec_acct_yes = ifelse(bank_data$securities_acct == "Yes",1,0)
bank_data$cd_acct_yes = ifelse(bank_data$cd_acct == "Yes",1,0)
bank_data$online_bank_yes = ifelse(bank_data$online_banking == "Yes",1,0)
bank_data$credit_card_yes = ifelse(bank_data$credit_card == "Yes",1,0)

#  education
#figure out the enumerations/categories for education
table(bank_data$education)

bank_data$education_Doc = ifelse(bank_data$education == "Doctorate",1,0)
bank_data$education_MS = ifelse(bank_data$education == "MS",1,0)

str(bank_data)
#Step 3: Set my target to contain factors
bank_data$personal_loan = as.factor(bank_data$personal_loan)

#Create a new normalized dataset
bank_data_norm = bank_data[,c(1,13:24)]
str(bank_data_norm)

#install.packages('caret')
library(caret)

set.seed(111)

k_values = expand.grid(k = c(1,3,5,7,9,11,13,15))

kNN_model = train(personal_loan ~., data = bank_data_norm, method = "knn",
                  trControl = trainControl(method = "cv",number = 5),
                  tuneGrid = k_values)
kNN_model

plot(kNN_model)

#---
#KNN Regression
corolla_data <- read.csv("C:/Users/108687/Desktop/BUSN305/BUSN305 Files/Lecture Notes/Week 9/corolla.csv")
str(corolla_data)
#Target: price (numerical - regression)
#Numerical predictors: age, mileage,horse_power,engine_vol, num_doors, weight
#Categorical predictors (yes/no): metalic,automatic
#Categorical predictors (enumerated): fuel_type

#Creating Dummies for categorical predictors
#install.packages("fastDummies")
library(fastDummies)

dummy_data = data.frame(dummy_cols(corolla_data,
                                   select_columns = c("metalic","automatic","fuel_type")))
View(dummy_data)

dummy_data = data.frame(dummy_cols(corolla_data,
                                   select_columns = c("metalic","automatic","fuel_type"),
                                   remove_first_dummy = TRUE))
View(dummy_data)

dummy_data = data.frame(dummy_cols(corolla_data,
                                   select_columns = c("metalic","automatic","fuel_type"),
                                   remove_first_dummy = TRUE,
                                   remove_selected_columns = TRUE))
View(dummy_data)

str(dummy_data)
#Normalize numerical predictors - Do not normalize our target
library(caret)

processed_data = preProcess(as.data.frame(dummy_data[,2:11]),
                            method = c("range"))
processed_data

corolla_data_norm = predict(processed_data,as.data.frame(dummy_data))
str(corolla_data_norm)

#only because our target is numerical we don't need to use as.factor
#to convert the target to a factor

#do kNN analysis on our normalized dataset
#if you haven't loaded caret, load it here

set.seed(4242)

k_values = expand.grid(k = c(1:10))

kNN_reg_model = train(price ~., data = corolla_data_norm,method = "knn",
                      trControl = trainControl(method = "cv",number = 5),
                      tuneGrid = k_values)

plot(kNN_reg_model)
kNN_reg_model$results

#Final Exam 

diabetes_data <- read.csv("C:/Users/Colin/Downloads/diabetes.csv")
View(diabetes_data)
str(diabetes_data)
count_table = table(diabetes_data$diabetes)
count_table
55/300
library(rpart)
library(rpart.plot)
library(gmodels)
CrossTable(diabetes_data$diabetes,diabetes_data$pregnancies)

diabetes_tree = rpart(diabetes~., data= diabetes_data, method= "class")
rpart.plot(diabetes_tree)

diabetes_predictions = predict(diabetes_tree,data = diabetes_data, type = "class")

CrossTable(diabetes_predictions, diabetes_data$diabetes)

(240+45)/300
45/(45+10)

walmart_data <- read.csv("C:/Users/Colin/Downloads/walmart_sales.csv")
   View(walmart_data)
 Walmart_mlr = lm(weekly_sales~., data= walmart_data )  
Walmart_mlr
summary(Walmart_mlr)

Walmart_mlr_2 = lm(weekly_sales~. -holiday_flag, data= walmart_data )
summary(Walmart_mlr_2)
Walmart_mlr_3 = lm(weekly_sales~.-holiday_flag -unemployment, data= walmart_data )
summary(Walmart_mlr_3)
residuals_mlr = resid(Walmart_mlr_3)
residuals_mlr
hist(residuals_mlr)
shapiro.test(residuals_mlr)

plot(walmart_data$weekly_sales,residuals_mlr)
abline(h=0, col = "red")


library(psych)
pairs.panels(walmart_data[c("temperature","fuel_price","cpi")])




#Knn.  

diabetes_data$pregnancies = (diabetes_data$pregnancies - min(diabetes_data$pregnancies)) / (max(diabetes_data$pregancies) - min(diabetes_data$pregnancies))
diabetes_data$fasting_glucose = (diabetes_data$fasting_glucose - min(diabetes_data$fasting_glucose)) / (max(diabetes_data$fasting_glucose) - min(diabetes_data$fasting_glucose))
diabetes_data$diastolic_blood_pressure = (diabetes_data$diastolic_blood_pressure - min(diabetes_data$diastolic_blood_pressure)) / (max(diabetes_data$diastolic_blood_pressure) - min(diabetes_data$diastolic_blood_pressure))
diabetes_data$skin_thickness = (diabetes_data$skin_thickness - min(diabetes_data$skin_thickness)) / (max(diabetes_data$skin_thickness) - min(diabetes_data$skin_thickness))
diabetes_data$insulin = (diabetes_data$insulin - min(diabetes_data$insulin)) / (max(diabetes_data$insulin) - min(diabetes_data$insulin))
diabetes_data$bmi = (diabetes_data$bmi - min(diabetes_data$bmi)) / (max(diabetes_data$bmi) - min(diabetes_data$bmi))
diabetes_data$age = (diabetes_data$age - min(diabetes_data$age)) / (max(diabetes_data$age) - min(diabetes_data$age))

diabetes_data$diabetes_yes = ifelse(diabetes_data$dabetes_yes== "Yes", 1, 0)

library(caret)
set.seed(420)
k_values = expand.grid(k = c(1,3,5,7,9,11,13,15,17,19))
train_control <- trainControl(method = "cv", number = 4)                                                 

kNN_model <- train(diabetes ~ ., 
                   data = diabetes_data,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(1, 19, 2)),
                   trControl = train_control)                                                 
plot(kNN_model)
summary(kNN_model)
kNN_model$results
