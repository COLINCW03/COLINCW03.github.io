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



