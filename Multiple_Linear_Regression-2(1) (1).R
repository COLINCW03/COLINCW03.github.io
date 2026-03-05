corolla_data <- read.csv("C:/Users/108687/Desktop/BUSN305/BUSN305 Files/Lecture Notes/Week 9/corolla.csv")
str(corolla_data)

corolla_mlr_model = lm(price ~ .,data = corolla_data)
corolla_mlr_model
summary(corolla_mlr_model)
#Adjusted R2 = 0.86

#Initial Linear Regression equation
#price = -6610.061216 - 133.038869 * age - 0.018609 * mileage + 36.940678 * horse_power
#        - 0.010774 * engine_vol - 52.136864 *num_doors + 19.816070 * weight
#        + 42.632391 * metalic + 186.222772 * automatic
#        if fuel_type = "Diesel", + 774.275409
#        if fuel_type = "Gas", + 1116.207211

# Start the stepwise backward elimination process
#Since engine_vol has the largest p value > 0.05 (0.91193) - eliminate engine_vol
corolla_mlr_model_1 = lm(price ~ . -engine_vol,data = corolla_data)
summary(corolla_mlr_model_1)
#Adjusted R2 = 0.8601

#Since metalic has the largest p value > 0.05 (0.66191) - eliminate metalic
corolla_mlr_model_2 = lm(price ~ . -engine_vol -metalic,data = corolla_data)
summary(corolla_mlr_model_2)
#Adjusted R2 = 0.8602

#Since automatic has the largest p value > 0.05 (0.39615) - eliminate automatic
corolla_mlr_model_3 = lm(price ~ . -engine_vol -metalic -automatic,data = corolla_data)
summary(corolla_mlr_model_3)
#Adjusted R2 = 0.8603

#Since num_doors has the largest p value > 0.05 (0.28566) - eliminate num_doors
corolla_mlr_model_4 = lm(price ~ . -engine_vol -metalic -automatic -num_doors,data = corolla_data)
summary(corolla_mlr_model_4)
#Adjusted R2 = 0.8602
#Final Linear Regression Equation
#price = -6573.013787 - 132.776372 * age - 0.018804 * mileage + 37.022934 * horse_power
#         + 19.614697 * weight
#         if fuel_type = "Diesel", + 764.908408
#         if fuel_type = "Gas", + 1093.858916

#Since fuel_typeDiesel has the largest p value > 0.05 (0.07704) - eliminate fuel_typeDiesel
corolla_mlr_model_5 = lm(price ~ . -engine_vol -metalic -automatic -num_doors -fuel_type,
                         data = corolla_data)
summary(corolla_mlr_model_5)
#Adjusted R2 = 0.8593
#Final Linear Regression Equation
#price = -4469.042147 - 131.93269 * age - 0.020579 * mileage + 39.212645  * horse_power
#         + 18.486387 * weight

age = 36
mileage = 55000
horse_power = 120
weight = 1200
price = -4469.042147 - 131.93269 * age - 0.020579 * mileage + 39.212645  * horse_power + 
  18.486387 * weight
price

#Get rid of e notation
options(scipen = 999)
#Residual Diagnostics
resid_4 = resid(corolla_mlr_model_4)
resid_4
resid_5 = resid(corolla_mlr_model_5)
resid_5

#Step 1: Check that the residual mean is 0
summary(resid_4)
summary(resid_5)
#Residual mean = 0

#Step 2: Check if the residuals are normally distributed
#H0: Residuals are normal
#H1: Residuals are not normal
shapiro.test(resid_4)
#p-value < 0.00000000000000022 < 0.05, Reject the null, Residuals are not normal
shapiro.test(resid_5)
#p-value < 0.00000000000000022 < 0.05, Reject the null, Residuals are not normal
#Conclude that residuals are not normal

#Step 3: Check that the residual standard deviation is constant (i.e., no heteroscedasticity)
plot(x = corolla_data$age, y = resid_5)
abline(h =0,col = "red")

plot(x = corolla_data$mileage, y = resid_5)
abline(h =0,col = "red")

plot(x = corolla_data$horse_power, y = resid_5)
abline(h =0,col = "red")

plot(x = corolla_data$weight, y = resid_5)
abline(h =0,col = "red")
#We do not believe there are any patterns and therefore error standard deviation is constant
#No heteroscedasticity

#Step 4: check for multicolinearity
#install.packages("psych")
library(psych)
pairs.panels(corolla_data[c("age","mileage","horse_power","weight")])
# age to mileage = 0.55, moderate correlation
# age to horse_power = -0.18, weak correlation
# age to weight = -0.5, moderate correlation
# mileage to horse_power = -0.35, moderate correlation
# mileage to weight = -0.08, weak correlation
# horse_power to weight = 0.09, weak correlation

#Even though we have 3 moderately correlated predictors, because none 
#are strong we can conclude that the model does not suffer 
#multicolinearity

#An alternate to pairs-panel is to look at the correlation coefficient
#for each predictor pair
cor(corolla_data$age,corolla_data$mileage)
