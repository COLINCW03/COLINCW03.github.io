AdAgency_data <- read.csv("C:/Users/Colin/Downloads/AdAgency.csv")
str(AdAgency_data)
View(AdAgency_data)
AdAgency__multiple_regression_model = lm(Profit~ ., data= AdAgency_data)
options(scipen = 999)
summary(AdAgency__multiple_regression_model)
table(AdAgency_data$Profit)
mean(AdAgency_data$Profit)
min(AdAgency_data$Profit)
max(AdAgency_data$Profit)
#creation of a dummy variable. (if needed)


AdAgency_data$CityLos_Angeles <- ifelse(AdAgency_data$City == "Los Angeles", 1, 0)
AdAgency_data$CityNew_York <- ifelse(AdAgency_data$City == "New York", 1, 0)
AdAgency_data$City <- NULL
head(AdAgency_data)
AdAgency__multiple_regression_model = lm(Profit~ ., data= AdAgency_data)
summary(AdAgency__multiple_regression_model)


hist(AdAgency_data$Profit)
shapiro.test(AdAgency_data$Profit)

#Seprate from homework
resid_multiple_regression = resid(AdAgency__multiple_regression_model)
resid_multiple_regression
hist(resid_multiple_regression)
shapiro.test(resid_multiple_regression)



#Elmination of varibales(Main issue)


AdAgency_data_mlr_1=lm(formula= Profit~ . - CityLos_Angeles, data= AdAgency_data)
summary(AdAgency_data_mlr_1)

AdAgency_data_mlr_2= lm(formula= Profit~ . -CityLos_Angeles -CityNew_York , data= AdAgency_data)
summary(AdAgency_data_mlr_2)

AdAgency_data_mlr_3= lm(formula= Profit~ .-CityLos_Angeles -CityNew_York -Social_Media, data= AdAgency_data)
summary(AdAgency_data_mlr_3)
#equation MLR

#Profit = 49.98305 + 0.79785*Print_media+ -.02891*Social_Media+ 0.03163*Outdoor_Ad
# +0.08335*CityLos_Angles+ -0.54267*CityNew_York
CityNew_York = 1
Print_Media = 145 
Social_Media = 230 
Outdoor_Ad = 180 

Profit <- 46.42502 + 0.78795 * Print_Media + 0.03449 * Outdoor_Ad
Profit

residuals_mlr = resid(AdAgency_data_mlr_3)
residuals_mlr

hist(residuals_mlr)
shapiro.test(residuals_mlr)

summary(residuals_mlr)

plot(AdAgency_data$Profit,residuals_mlr)
abline(h=0, col = "red")

#Final residuals vs predictors


plot(AdAgency_data$Print_Media, residuals_mlr)
abline(h=0, col= "red")
plot(AdAgency_data$Outdoor_Ad, residuals_mlr)
abline(h=0, col= "red")

shapiro.test(residuals_mlr)

#correlation matrix. 
install.packages("psych")
library(psych)
pairs.panels(AdAgency_data[c("Profit","Print_Media","Outdoor_Ad")])


weekly_sales= -3.95 + 2.275571*-10 + -9.65710*fuel_price+5.0003*cpi
