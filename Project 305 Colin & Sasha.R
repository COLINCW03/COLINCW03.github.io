telecom_data <- read.csv("C:/Users/Colin/Downloads/telecom.csv")
  View(telecom_data)
  
#1 Binning tenure into bins
  
 
breaks_tenure <- c(0, 12, 24, 36, 48, 60, 72)
labels_tenure <- c("0-12 months", "12-24 months", "24-36 months",
                     "36-48 months", "48-60 months", "60-72 months")
telecom_data$binned_tenure <- cut(telecom_data$tenure,
  breaks = breaks_tenure,
  labels = labels_tenure,
  right = FALSE,
  include.lowest = TRUE)
# Step 2: Binning 'monthly_charges' into 8 bins, $25 intervals
breaks_charges <- c(0, 25, 50, 75, 100, 125, 150, 175, 200)
labels_charges <- c("0-$25", "$25-$50", "$50-$75", "$75-$100",
                      "$100-$125", "$125-$150", "$150-$175", "$175-$200")
#Step #3
telecom_data$binned_monthly_charges <- cut(telecom_data$monthly_charges,
  breaks = breaks_charges,
  labels = labels_charges,
  right = FALSE,
  include.lowest = TRUE)
  
# Step 4: Export the updated data frame to a new CSV file
write.csv(telecom_data, "telecom_binned.csv", row.names = FALSE)
  
#2 cross tables. 

# Frequency of churn
churn_counts <- table(telecom_data$churn)
print(churn_counts)

# Proportion of churn
churn_percentages <- prop.table(churn_counts) * 100
print(round(churn_percentages, 2))


# Chrun vs. Contract
prop.table(table(telecom_data$churn, telecom_data$contract))
#chrun vs Internet 
prop.table(table(telecom_data$churn, telecom_data$internet))
#Churn vs Phone Service 
prop.table(table(telecom_data$churn, telecom_data$phone_service))
#Churn vs Security service 
prop.table(table (telecom_data$churn, telecom_data$security))
#Churn vs Binned Tenure 
prop.table(table(telecom_data$churn, telecom_data$binned_tenure))
#Churn vs Binned Monthly Charges. 
prop.table(table(telecom_data$churn, telecom_data$binned_monthly_charges))

#3
#Setting the seed value
set.seed(42)
#generate random smaple of 80%
train_indices <- sample(1:nrow(telecom_data), size = 0.8 * nrow(telecom_data))
#create training and validation set 
training_set = telecom_data[train_indices, ]
validation_set = telecom_data[-train_indices, ]
#Doubling checking the split. 
nrow(training_set)  # Should be ~4000
nrow(validation_set)  # Should be ~1000
# 4
library(rpart)
library(rpart.plot)
library(gmodels)
telecom_tree_model = rpart(churn~ . -tenure -monthly_charges, data = 
     training_set ,method = "class")
rpart.plot(telecom_tree_model)
#pred
train_pred = predict(telecom_tree_model, training_set, type = "class")
# confusion matrix. 
CrossTable(train_pred,training_set$churn)
 
accuracy = (927+2496)/4000
accuracy
misclass_rate = (117+460)/4000
misclass_rate
sensitivity = 927/(927+460)
sensitivity
specificity = 2496/(2496+117)
specificity

validation_pred = predict(telecom_tree_model, validation_set, type = "class")
CrossTable(validation_pred,validation_set$churn)

accuracy_val = (634+216)/1000
accuracy_val
misclass_rate_val = (28+122)/1000
misclass_rate_val
sensitivity_val = 216/(216+122)
sensitivity_val
specificity_val = 634/(28+634)
specificity_val
