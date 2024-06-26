### STEP 1: GET DATA ###

#Under gender: male is represented by 1, female by 2 and other by 0
#Under ever_married: no is represented by 3 and yes by 4
#Under work_type: children is represented by 5, Govt_job by 6, Never_worked by 7, Private by 8 and Self-employed by 9
#Under residence_type: Urban is represented by 10 and Rural by 11
#Under smoking_status: formely smoked is represented by 12, never smoked by 13, smokes by 14 and blanks by blanks

## Loading the Data to R ##
library(readxl)
install.packages("vctrs")
library(vctrs)
healthcare_data <- read_excel("C:/Users/wawer/Downloads/stroke prediction.xlsx")
head(healthcare_data)



### STEP 2: EXPLORING AND PREPARING THE DATA ###

## Explore the structure of the data ##
str(healthcare_data)

## Handle missing values if any ##
any(is.na(healthcare_data))
# Impute missing values with the mean of the variable
healthcare_data$smoking_status[is.na(healthcare_data$smoking_status)] <- mean(healthcare_data$smoking_status, na.rm = TRUE)
# Create a binary indicator variable for missing values
healthcare_data$smoking_status_is_missing <- ifelse(is.na(healthcare_data$smoking_status), 15, 0)

## Conduct summary statistics for the data ##
summary(healthcare_data)

## Run a plot for every predictor variable with our response variable, stroke ##
library(ggplot2)
# Replace 'response_variable' with your actual response variable name
response_variable <- "stroke"
# Replace 'predictor_variables' with a vector of your predictor variable names
predictor_variables <- c("gender","age","bmi","hypertension","heart_disease","ever_married","work_type","residence_type","avg_glucose_level","smoking_status")
# Create a scatterplot for each predictor variable against the response variable
for (predictor in predictor_variables) {
  plot_title <- paste("Scatterplot of", predictor, "vs.", response_variable)
  
  p <- ggplot(healthcare_data, aes_string(x = predictor, y = response_variable)) + 
    geom_point() + 
    labs(title= plot_title)
  
  print (p)
}
# Replace 'response_variable' with the actual name of your response variable
response_variable <- healthcare_data$stroke

## Draw a histogram of the response variable ##
hist(healthcare_data$stroke, main = "Stroke Histogram", xlab = "Stroke (1 = Yes, 2 = No)", col = "lightblue")

## Conduct a correlation analysis of the variables ##
numeric_data <- healthcare_data[, sapply(healthcare_data, is.numeric)]
correlation_matrix <- cor(numeric_data)
library(reshape2)
correlation_data <- melt(correlation_matrix)

## Visualize the relationship between the features using a scatterplot matrix function, or any other appropriate function ##
pairs(healthcare_data[, c("gender","age","bmi","hypertension","heart_disease","ever_married","work_type","residence_type","avg_glucose_level","smoking_status")])

## Encode the categorical variables using appropriate methods. Check that the target variable meets the specifications of the logistic model ##
healthcare_data$gender <- as.factor(healthcare_data$gender)
healthcare_data$ever_married <- as.factor(healthcare_data$ever_married)
healthcare_data$work_type <- as.factor(healthcare_data$work_type)
healthcare_data$residence_type <- as.factor(healthcare_data$residence_type)
healthcare_data$smoking_status <- as.factor(healthcare_data$smoking_status)


### STEP 3: TRAINING THE MODEL ###

## Split the dataset into the training set (70%) and the testing set (30%) ##
# Load the caTools package
library(caTools)
# Set a random seed for reproducibility
set.seed(123)
# Create a binary vector for splitting
split_data <- sample.split(healthcare_data$stroke, SplitRatio = 0.7)
# Create the training and testing sets
training_data <- healthcare_data[split_data, ]
testing_data <- healthcare_data[!split_data, ]

## Fit a logistic regression model on the data ##
# Fit a logistic regression model
logistic_model <- glm(stroke ~ gender + age + hypertension + heart_disease + ever_married + work_type + residence_type + avg_glucose_level + bmi + smoking_status, data = training_data, family = binomial)
# View the model summary
summary(logistic_model)

## Interpret the coefficients of the model ##
# In the case of gender, both coefficients are positive, but they are not statistically significant (p-values > 0.05). This suggests that gender may not be a significant predictor of stroke in this model.
# A 1-unit increase in age is associated with a 0.074 increase in the log-odds of having a stroke. This is a statistically significant and positive coefficient, indicating that as age increases, the likelihood of having a stroke also increases.
# Having hypertension is associated with a 0.436 increase in the log-odds of having a stroke. This is a statistically significant and positive coefficient, suggesting that individuals with hypertension have a higher likelihood of having a stroke.
# Having heart disease is associated with a 0.599 increase in the log-odds of having a stroke. This is a statistically significant and positive coefficient, indicating that individuals with heart disease have a higher likelihood of having a stroke.
# The negative coefficient indicates that being "Never Married" is associated with a -0.196 decrease in the log-odds of having a stroke. However, this coefficient is not statistically significant.
# Most of the coefficients associated with the differentt work_types are not statistically significant, indicating that work type may not be a significant predictor in this model.
# The coefficient of "residence_type" variable suggests that residing in an "Urban" area is associated with a 0.067 increase in the log-odds of having a stroke. However, this coefficient is not statistically significant.
# A one-unit increase in the average glucose level is associated with a 0.0037 increase in the log-odds of having a stroke. This is a statistically significant and positive coefficient, suggesting that higher average glucose levels are associated with a higher likelihood of having a stroke.
# A one-unit increase in BMI is associated with a -0.0126 decrease in the log-odds of having a stroke. However, this coefficient is not statistically significant.
# These coefficients correspond to different categories of the "smoking_status" variable. Only the coefficient for "smoking_status14" is statistically significant, suggesting that individuals with this smoking status have a 0.376 increase in the log-odds of having a stroke.
# In summary, the logistic regression model indicates that age, hypertension, heart disease, and average glucose level are statistically significant predictors of stroke, while other variables like gender, work type, residence type, ever married status, BMI, and certain smoking statuses may not be significant predictors.

### STEP 4: EVALUATING MODEL PERFORMANCE ###

## Evaluate the model's performance on the testing data using appropriate metrics ##
library(pROC)
# Make predictions on the testing data
predictions <- predict(logistic_model, newdata = testing_data, type = "response")

# Calculate the accuracy, precision, recall, F1-score, and ROC-AUC
accuracy <- sum(predictions > 0.5) / nrow(testing_data)
precision <- sum(predictions > 0.5 & testing_data$stroke == 1) / sum(predictions > 0.5)
recall <- sum(predictions > 0.5 & testing_data$stroke == 1) / sum(testing_data$stroke == 1)
F1_score <- 2 * (precision * recall) / (precision + recall)
roc_auc <- roc(testing_data$stroke, predictions)$auc

# Print the performance metrics
print(paste("Accuracy: ", accuracy))
print(paste("Precision: ", precision))
print(paste("Recall: ", recall))
print(paste("F1-score: ", F1_score))
print(paste("ROC-AUC: ", roc_auc))


### STEP 5: IMPROVING MODEL PERFROMANCE ###
install.packages("glmnet")
library(glmnet)
