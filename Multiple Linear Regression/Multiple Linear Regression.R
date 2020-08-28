# Individual Assignment 2: Simple and Multiple Linear Regression by Shimony Agrawal 
# Seed Assigned: 10

# Install packages required for the data analysis
install.packages("DBI")
install.packages("odbc")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("ISLR")
install.packages("caret")
install.packages("forecast")
install.packages("corrplot")
install.packages ("visualize")

library(DBI)
library(odbc)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ISLR)
library(caret)
library(forecast)
library(corrplot)
library(visualize)

## Part 1: Simple Linear Regression 

# Task 1: For this assignment, we will use the dataset Carseats, which comes from the ISLR package. After you have installed ISLR, and used the library() function to bring this package into your environment. 
  
data(Carseats)

# Task 2: Using ggplot,create a scatterplot that depicts the Sales variable on the y-axis and the Price variable on the x-axis. Add a best-fit line to this scatterplot.

ggplot(data = Carseats, aes(x = Price, y = Sales)) + 
  geom_point(color = 'blue', size = 2, shape = 20)  + 
  geom_smooth(method = 'lm', se = FALSE, color = 'khaki') +
  ggtitle("Relationship between Sales and Price of Carseats") +
  xlab("Price") +
  ylab("Sales")

# Task 3: Find the correlation between these variables. Then, use cor.test() to see whether this correlation is significant.

cor.test(Carseats$Sales, Carseats$Price, method = 'pearson')

# Task 4: Using your assigned seed value, create a data partition. Assign approximately 60% of the records to your training set, and the other 40% to your validation set.

nrow(Carseats) * .6  # To divide the dataset into 60% train and 40% valid 
set.seed(10) # Assigned Seed Value 
Carseats_sample <- sample_n(Carseats, 400) 
Train <- slice(Carseats_sample, 1:240)
Valid <- slice(Carseats_sample, 241:400) 

# Task 5: Using your training set, create a simple linear regression model, with Sales as your outcome variable and Price as your input variable. 

carseat_lm <- lm(Sales ~ Price, data = Train)
summary(carseat_lm)

# Task 6, 7 : Refer to the write-up. 

# Task 8: Using the accuracy() function from the forecast package, assess the accuracy of your model against both the training set and the validation set.

carseat_pred1 <- predict(carseat_lm, Valid)
accuracy(carseat_pred1, Valid$Sales)

2.411565 / mean(Valid$Sales)

carseat_pred2 <- predict(carseat_lm, Train)
accuracy(carseat_pred2, Train$Sales)

2.601682 / mean(Train$Sales)

## Part 2: Multiple Linear Regression 

# Task 1: Build a correlation table in R that depicts the correlations among all of the numerical variables that you might use as predictors (use your training set to build this).

Train_numeric <- subset(Train, select = -c (ShelveLoc, Urban, US))

Train.cor = cor(Train_numeric)
corrplot(Train.cor)

# Task 2: Refer to the write-up. 

# Task 3: Using backward elimination, build a multiple regression model with the data in your training set, with the goal of predicting the Sales variable.

carseat_mlr <- lm(Sales ~ ., data = Train)

carseatmlr_step <- step(carseat_mlr, direction = "backward")

summary(carseatmlr_step)  

# Task 4: Using the variables that you will keep, build a multiple linear regression model. Show a summary of your multiple regression model.

carseat_mlr1 <- lm(Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + Age, data = Train)
summary(carseat_mlr1)


# Task 5 (a, b, c): What is the total sum of squares for your model? (SST). This can be found by  What is the total sum of squares due to regression for your model? (SSR).What is your SSR / SST? Where can you also see this value in the summary of your regression model?


# SST 

Train$outcome_diff <- Train$Sales - mean(Train$Sales)
Train$squared_diff <- Train$outcome_diff^2
sum(Train$squared_diff)

# SSR 

Train$explained <- carseat_mlr1$fitted.values - mean(Train$Sales)
Train$squared_explained <- Train$explained^2
sum(Train$squared_explained)

# SSR / SST = R Square

1809.229 / 2042.488

# Task 6: Getting from a t-value to a p-value. Choose one of the predictors from your model. What is the t-value for that predictor? Using the visualize.t() function from the visualize package, create a plot of the t-distribution that shows the distribution for that t-value and the number of degrees of freedom in your model.

visualize.t(stat=c(-5.899, 5.869), df=232, section="bounded") # t-value of Income: 5.869

# Task 7: Make up a fictional retail location, and assign attributes to it for each of the predictors in your model. What does your model predict that this location’s sales will be? To answer this, you can use a function in R or just explain it using the equation and some simple math.

fiction <- data.frame(CompPrice = 150,
                      Income = 100 ,
                      Advertising = 25 , 
                      Price = 170 , 
                      ShelveLoc = 'Medium', 
                      Age = 30)

pred <- predict (carseat_mlr1, fiction)

# Task 8: Using the accuracy() function from the forecast package, assess the accuracy of your model against both the training set and the validation set.

carseatmlr_pred1 <- predict(carseat_mlr1, Valid)
accuracy(carseatmlr_pred1, Valid$Sales)

1.058035 / mean(Valid$Sales)

carseatmlr_pred2 <- predict(carseat_mlr1, Train)
accuracy(carseatmlr_pred2, Train$Sales)

0.985857 / mean(Train$Sales)




