marketing_data <- read.csv("C:/Users/Colin/Downloads/marketing_data.csv")
   View(marketing_data)
  str(marketing_data)
#1) Normalize the 5 numerical predictors and create new variables in your dataset to contain the normalized variables. For consistency, append _norm to the end of the variable name which you are normalizing when you create the new column.
  marketing_data$Num_Bank_Accounts_Open = (marketing_data$Num_Bank_Accounts_Open - min(marketing_data$Num_Bank_Accounts_Open)) / (max(marketing_data$Num_Bank_Accounts_Open) - min(marketing_data$Num_Bank_Accounts_Open))
  marketing_data$Num_Credit_Cards_Held = (marketing_data$Num_Credit_Cards_Held - min(marketing_data$Num_Credit_Cards_Held)) / (max(marketing_data$Num_Credit_Cards_Held) - min(marketing_data$Num_Credit_Cards_Held))
  marketing_data$Num_Homes_Owned = (marketing_data$Num_Homes_Owned - min(marketing_data$Num_Homes_Owned)) / (max(marketing_data$Num_Homes_Owned) - min(marketing_data$Num_Homes_Owned))
  marketing_data$Household_Size = (marketing_data$Household_Size - min(marketing_data$Household_Size)) / (max(marketing_data$Household_Size) - min(marketing_data$Household_Size))
  marketing_data$Average_CC_Balance = (marketing_data$Average_CC_Balance - min(marketing_data$Average_CC_Balance)) / (max(marketing_data$Average_CC_Balance) - min(marketing_data$Average_CC_Balance))
  
#2) Create dummy variables for the 2 Yes/No predictors. For consistency, append _yes to the end of the variable name when you create the new column.
  marketing_data$Own_Home_yes = ifelse(marketing_data$Own_Your_Home== "Yes", 1, 0)
  marketing_data$Overdraft_Protection = ifelse(marketing_data$Overdraft_Protection == "Yes", 1, 0)
  
#3)Create dummy variables for the 4 remaining categorical predictors.
marketing_data$Reward_airmiles <- ifelse(marketing_data$Reward == "Air Miles", 1, 0)
marketing_data$Reward_cashback <- ifelse(marketing_data$Reward == "Cash Back", 1, 0)

  
# Mailer_Type (Assuming levels: 'Letter', 'Postcard')
marketing_data$Mailer_Type_letter <- ifelse(marketing_data$Mailer_Type == "Letter", 1, 0)

  
# Income_Level (Assuming levels: 'Low', 'Medium', 'High')
marketing_data$Income_Level_low <- ifelse(marketing_data$Income_Level == "Low", 1, 0)
marketing_data$Income_Level_medium <- ifelse(marketing_data$Income_Level == "Medium", 1, 0)


  
# Credit_Rating (Assuming levels: 'Low', 'Medium', 'High')
marketing_data$Credit_Rating_low <- ifelse(marketing_data$Credit_Rating == "Low", 1, 0)
marketing_data$Credit_Rating_medium <- ifelse(marketing_data$Credit_Rating == "Medium", 1, 0)


  
  
#4)Create a new dataset named market_data_norm which contains your target variable and the 14 columns that you have created in steps 1-3. Before you do that, don't forget to convert your target (Offer_Accepted) to be a factor.
marketing_data$Offer_Accepted = as.factor(marketing_data$Offer_Accepted)
str(marketing_data)

marketing_data_norm = marketing_data[,c(1,13:26)]

market_data_norm <- marketing_data[, c("Offer_Accepted",
                                      "Num_Bank_Accounts_Open",
                                      "Num_Credit_Cards_Held",
                                      "Num_Homes_Owned",
                                      "Household_Size",
                                      "Average_CC_Balance",
                                      "Own_Home_yes",
                                      "Overdraft_Protection",
                                      "Reward_airmiles",
                                      "Reward_cashback",
                                      "Mailer_Type_letter",
                                      "Income_Level_low",
                                      "Income_Level_medium",
                                      "Credit_Rating_low",
                                      "Credit_Rating_medium"
                                     )]
  

  
#5) Create your k-nearest neighbor model. (don't forget to load the caret library)
  
#Set your seed value to 42
#In this exercise we will evaluate the odd neighbors from 1 to 17 (in other words, 1, 3, 5, 7, 9, 11, 13, 15, 17)  We will again use 5-fold cross-validation to train our model
#Plot a chart of your k-nearest neighbor model
#Find the k-fold Accuracy
library(caret)
set.seed(42)
k_values = expand.grid(k = c( 1, 3, 5, 7, 9, 11, 13, 15, 17))
train_control <- trainControl(method = "cv", number = 5)

knn_model <- train(Offer_Accepted ~ ., 
                   data = market_data_norm,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(1, 17, 2)),
                   trControl = train_control)

plot(knn_model)

knn_model$results
#6)What k value for nearest neighbors produces the highest average accuracy?
 # 9
  
#7)What is the best average accuracy value?
 # 0.9331666 
  
#8)Recall that the marketing_data dataset analyzed using a classification tree resulted in validation set accuracy of 0.9438889. Based on the best k-Nearest neighbor average accuracy, which method would provide better predictions?

#Classification tree