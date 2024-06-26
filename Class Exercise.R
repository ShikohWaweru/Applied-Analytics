# STEP 1: COLLECTING THE DATA
data(mtcars)
attach(mtcars)

# STEP 2: EXPLORING AND PREPARING THE DATA

## The structure of the data
str(mtcars)

## summary statistics for the data
summary(mtcars)

## A plot for every predictor variable with our response variable, MPG.
library(ggplot2)
# Replace 'response_variable' with your actual response variable name
response_variable <- "mpg"
# Replace 'predictor_variables' with a vector of your predictor variable names
predictor_variables <- c("cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb")
# Create a barplot for each predictor variable against the response variable
for (predictor in predictor_variables) {
  plot_title <- paste("Barplot of", predictor, "vs.", response_variable)
  
  p <- ggplot(mtcars, aes_string(x = predictor, y = response_variable)) + 
    geom_point() + 
    labs(title= plot_title)
  
  print (p)
}
# Replace 'response_variable' with the actual name of your response variable
response_variable <- mtcars$mpg

## Create a histogram
hist(response_variable,
     main = "Histogram of Response Variable",
     Xlab = "Values of Response Variable",
     ylab = "Frequency",
     col = "blue",
     border = "black"
     )

## Correlation analysis of the variables
correlation_matrix <- cor(numeric_data)
library(reshape2)
correlation_data <- melt(correlation_matrix)

## Visualize the relationship between the features using a scatterplot matrix function

#Create a scatterplot matrix
pairs(mtcars)
# Create a scatterplot matrix for a subset of variables
variables_of_interest <- c("mpg", "cyl", "disp", "hp", "wt", "qsec", "drat", "vs", "am", "gear", "carb" )
pairs(mtcars[, variables_of_interest])
pairs(mtcars, col = "blue", pch = 20, labels = c("mpg", "cyl", "disp", "hp", "wt", "qsec", "drat", "vs", "am", "gear", "carb"))

## Create an enhanced scatterplot matrix can be created with the pairs.panels() function
install.packages("psych")
library(psych)
# Replace the variable names as needed
vars_of_interest <- c("mpg", "cyl", "disp", "hp", "wt", "qsec", "drat", "vs", "am", "gear", "carb")
# Create an enhanced scatterplot matrix
pairs.panels(mtcars[vars_of_interest], lm = TRUE, ellipses = TRUE, density = TRUE)


## STEP 3: TRAINING THE MODEL
# Fit a linear regression model on the data
model <- lm(mpg ~ cyl + disp + hp + wt + qsec + drat + vs + am + gear + carb, data = mtcars)


## STEP 4: EVALUATING MODEL PERFORMANCE
summary(model)

## STEP 5: IMPROVING MODEL PERFORMANCE
#Add nonlinear relationships to any 2 variables of your choosing
# Fit a quadratic model between cyl and sisp
model2 <- lm(disp ~ poly(cyl, 2), data = mtcars)

