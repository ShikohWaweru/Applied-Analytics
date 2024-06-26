#Reading in the Data

library(readxl)
credit_Class_Data_to_R <- read_excel("C:/Users/wawer/Downloads/Financial Engineering/Year 3 Financial Engineering/Sem 2 Year 3/Applied analytics/credit (Class Data) to R.xlsx")
View(credit_Class_Data_to_R)
head(credit_Class_Data_to_R)

#Data Preprocessing

##Overview of the data

class(credit_Class_Data_to_R)
summary(credit_Class_Data_to_R)

##Sample Data Plots
par(mfrow = c(2,2))

# frequency plots of numeric variables 
hist(credit_Class_Data_to_R$default, main = "Plot of Debtors Default in Credit History", xlab = "Default")
hist(credit_Class_Data_to_R$age, main = "Plot of Ages of Debtors in Credit History", xlab = "Age")

# frequency plots of character variables 
plot(as.factor(credit_Class_Data_to_R$job), main = "Plot of Job Groups of Debtors in Credit History", xlab = "Job Groups")
plot(as.factor(credit_Class_Data_to_R$housing), main = "Plot of Housing of Debtors in Credit History", xlab = "Housing")

## Character to Numeric Data

# Factor out numeric columns
num_data <- credit_Class_Data_to_R[ , 1:8]
head(num_data )

# function to change categorical character data to numeric data

checking_balance <- factor(credit_Class_Data_to_R$checking_balance, ordered = TRUE)
head(checking_balance)

head(as.numeric(checking_balance))

# generalizing the function to apply to all columns

char_cols <- credit_Class_Data_to_R[ , 9:ncol(credit_Class_Data_to_R)]

fact_function <- function(x){
  return(as.numeric(factor(x, ordered = TRUE)))
}

char_data <- lapply(char_cols, fact_function)
char_data <- as.data.frame(char_data, col.names = colnames(char_cols))
head(char_data)

# the final dataset

data <- cbind.data.frame(num_data, char_data)
head(data)

# Fitting a linear regression

linear_model <- lm(data$default~., data = data)

summary(linear_model)

###Checking for homoskedasticity

par(mfrow = c(2,2))
plot(linear_model)


# Fitting a Logistic Regression Model 

#the logistic model 

#This is given that our response variable is binary where 1 represents default event, while 2 (now changed to 0) represents no default. 

## Train - Test Split

## Manually (80 - 20)

# we have 1000 data points (rows) on 21 variables (columns)
# 80% of this is 800 rows

train_data <- data[1:800, ]
test_data <- data[801:1000, ]

## Fitting a linear model on the train data

training_logistic_model <- glm(train_data$default~., data = train_data, family = "binomial")

summary(training_logistic_model)

## Automatic Split (Much better and functional)

# for logistic regression
install.packages("caTools") 
library(caTools)
# for model evaluation
install.packages("ROCR") 
library(ROCR)

split_data <- sample.split(data, SplitRatio = 0.8)

# split ration is 80:20

head(split_data)

traindata <- subset(data, split_data == "TRUE")  # picks datapoints that return TRUE
testdata <- subset(data, split_data == "FALSE")

### Fitting the Logistic Model

# Logistic Model on the Training model
logistic_model <- glm(traindata$default~., data = traindata, family = "binomial")
summary(logistic_model)


train_predict <- predict(logistic_model, testdata, type = "response")
head(train_predict)

# change output to probabilities
predicted_probs <- ifelse(train_predict >0.5, 1, 0)
head(predicted_probs)

table(predicted_probs)
table(testdata$default)
