# Loading the Data to R #
library(readxl)
scraped_data <- read_excel("C:/Users/wawer/Downloads/DATA.xlsx")
head(scraped_data)

### QUESTION ONE ####

## Part A ##

## The structure of the data
str(scraped_data)
## Handling any missing values if any
any(is.na(scraped_data))
## summary statistics for the data
summary(scraped_data)
## A plot for every predictor variable with our response variable, price.
library(ggplot2)
# Replace 'response_variable' with your actual response variable name
response_variable <- "model"
# Replace 'predictor_variables' with a vector of your predictor variable names
predictor_variables <- c("year","price","transmission","mileage","fuelType","tax","mpg","engineSize")
# Create a scatterplot for each predictor variable against the response variable
for (predictor in predictor_variables) {
  plot_title <- paste("Scatterplot of", predictor, "vs.", response_variable)
  
  p <- ggplot(scraped_data, aes_string(x = predictor, y = response_variable)) + 
    geom_point() + 
    labs(title= plot_title)
  
  print (p)
}
# Replace 'response_variable' with the actual name of your response variable
response_variable <- scraped_data$model
## Correlation analysis of the variables
numeric_data <- scraped_data[, sapply(scraped_data, is.numeric)]
correlation_matrix <- cor(numeric_data)
library(reshape2)
correlation_data <- melt(correlation_matrix)  

## Part B ##

install.packages("GGally")
library(GGally)
# Create a SPLOM
pairs_plot <- ggpairs(data =scraped_data ,
                      columns = c("mileage", "year", "engineSize", "fuelType"),
                      title = "Scatterplot Matrix")
# Display the SPLOM
print(pairs_plot)
#Comment on your results 



## Part C ##

#TRAINING THE MODEL: Fit a linear regression model on the data
model_one <- lm(price ~ year + transmission + mileage + fuelType + tax + mpg + engineSize, data = scraped_data )
summary(model)
#Comment on the coefficients of your model and their implications

#Comment on the performance of your model



## Part D ##

# Calculate the mean price
mean_price <- mean(scraped_data$price)
# Create binary variables for above/below mean price
scraped_data$above_mean_price <- ifelse(scraped_data$price > mean_price, 1, 0)
# Build a modified linear regression model with interaction terms
model_two <- lm(price ~ mpg * engineSize + year * mileage + above_mean_price, data = scraped_data)
#Comment on the coefficients of your model and their implications

#comment on the performance of your model and whether the consultant’s advice was valuable



### QUESTION TWO ###

# Loading the data #
library(readxl)
health_data <- read_excel("C:/Users/wawer/Downloads/heart_failure_clinical_records_dataset.xlsx")
head(health_data)

## Part A ##
## The structure of the data
str(health_data)
## Handling any missing values if any
any(is.na(health_data))
## summary statistics for the data
summary(health_data)
## A plot for every predictor variable with our response variable, price.
library(ggplot2)
# Replace 'target_variable' with your actual response variable name
target_variable <- "failure_event"
# Replace 'cause_variables' with a vector of your predictor variable names
cause_variables <- c("age","anaemia","creatinine_phosphokinase","diabetes","ejection_fraction","high_blood_pressure","platelets","serum_creatinine","serum_sodium","sex","smoking","time")
# Create a scatterplot for each predictor variable against the response variable
for (predictor in cause_variables) {
  plot_title <- paste("Scatterplot of", predictor, "vs.", target_variable)
  
  q <- ggplot(health_data, aes_string(x = predictor, y = target_variable)) + 
    geom_point() + 
    labs(title= plot_title)
  
  print (q)
}
# Replace 'response_variable' with the actual name of your response variable
target_variable <- health_data$failure_event
## Correlation analysis of the variables
correlation_matrix <- cor(health_data)
library(reshape2)
correlation_data <- melt(correlation_matrix)  

# Create a SPLOM
library(GGally)
pairs_plot2 <- ggpairs(data =health_data ,
                      columns = c("high_blood_pressure","age","smoking","anaemia"),
                      title = "Scatterplot Matrix")
# Display the SPLOM
print(pairs_plot2)
#Comment on your results 


## Part B ##
# Set a specific seed value
set.seed(1000)
index <- sample(1:nrow(health_data), nrow(health_data) * 0.7)
train_data <- health_data[index, ]
test_data <- health_data[-index, ]
install.packages("recipes")
library(caret)
model_formula <- as.formula(failure_event ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking + time)
logistic_model <- glm(model_formula, data = health_data, family = "binomial")
summary(logistic_model)


## Part C ##

# Make predictions on the test dataset using the trained model
predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to class labels based on a threshold
# For binary classification, a common threshold is 0.5
predicted_classes <- ifelse(predictions > 0.5, "Class1", "Class2")

# Create a confusion matrix
install.packages("recipes")
library(recipes)
library(caret)
confusion <- confusionMatrix(data = predicted_classes, reference = test_data$actual_class)

# Print the confusion matrix
print(confusion)
