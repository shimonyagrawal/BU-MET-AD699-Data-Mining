# Individual Assignment 3: Classification using K-Nearest Neighbours and Naive Bayes by Shimony Agrawal 
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
install.packages("FNN")
install.packages("e1071")

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
library(FNN)
library(e1071)

## Part 1: K-Nearest Neighbours

# Task 1: Read the file. 

employees = read.csv("/Users/shimonyagrawal/Desktop/Grad /Summer 2/AD699_Data Mining/RStudio/Assignment 3/employees.csv")
employees <- employees[,c(2, 1, 3:12)]

# Task 2: Data type of variables 

str(employees)

employees$Attrition <- as.factor(employees$Attrition)

str(employees)

# Task 3:Are there any NAs in this dataset? Show the code that you used to find this out. If there are any NA values in any particular column, replace them with the median value for that column.

anyNA(employees$Attrition) # No NA values in the dataset. 

# Task 4: Filter the original employees dataframe to create two new temporary dataframes. One of these dataframes should contain the records for employees who left the company, and the other dataframe should contain the records for employees who did not leave the company.Call the summary() function on each of these two dataframes that you just made. 

employees_left <- employees %>%
  filter (Attrition == 'Yes')

employees_stayed <- employees %>%
  filter (Attrition == 'No')

summary(employees_left)
summary(employees_stayed)

# Task 5: Using your assigned seed value (from Assignment 2), partition your entire dataset into training (60%) and validation (40%) sets.

nrow(employees) * .6  # To divide the dataset into 60% train and 40% valid 
set.seed(10) # Assigned Seed Value 
employees_sample <- sample_n(employees, 1470) 
Train_KNN <- slice(employees_sample, 1:882)
Valid_KNN <- slice(employees_sample, 883:1470) 

# Task 6a: Emma 

# Task 6b:Use the runif() function to give your person values for each of the numeric predictor attributes. Use the min and max values from your training set as the lower and upper boundaries for runif().


Emma_runif <- data.frame(Age = runif(1, min(Train_KNN$Age), max(Train_KNN$Age)), 
                   DistanceFromHome = runif(1, min(Train_KNN$DistanceFromHome), max(Train_KNN$DistanceFromHome)), 
                   MonthlyIncome = runif(1, min(Train_KNN$MonthlyIncome), max(Train_KNN$MonthlyIncome)),
                   NumCompaniesWorked = runif(1, min(Train_KNN$NumCompaniesWorked), max(Train_KNN$NumCompaniesWorked)),
                   PercentSalaryHike = runif(1, min(Train_KNN$PercentSalaryHike), max(Train_KNN$PercentSalaryHike)),
                   TotalWorkingYears = runif(1, min(Train_KNN$TotalWorkingYears), max(Train_KNN$TotalWorkingYears)),
                   TrainingTimesLastYear = runif(1, min(Train_KNN$TrainingTimesLastYear), max(Train_KNN$TrainingTimesLastYear)),
                   YearsAtCompany = runif(1, min(Train_KNN$YearsAtCompany), max(Train_KNN$YearsAtCompany)),
                   YearsInCurrentRole = runif(1, min(Train_KNN$YearsInCurrentRole), max(Train_KNN$YearsInCurrentRole)),
                   YearsSinceLastPromotion = runif(1, min(Train_KNN$YearsSinceLastPromotion), max(Train_KNN$YearsSinceLastPromotion)),
                   YearsWithCurrManager = runif(1, min(Train_KNN$YearsWithCurrManager), max(Train_KNN$YearsWithCurrManager)))

Emma <- data.frame(Age = 50.94084, 
                   DistanceFromHome = 5.155811,
                   MonthlyIncome = 5233.086,
                   NumCompaniesWorked = 3.535845,
                   PercentSalaryHike = 24.89131,
                   TotalWorkingYears = 26.91134,
                   TrainingTimesLastYear = 3.311238,
                   YearsAtCompany = 38.72965,
                   YearsInCurrentRole = 7.547142,
                   YearsSinceLastPromotion = 3.1872,
                   YearsWithCurrManager = 12.10349)

# Task 7: Normalize your data using the preProcess() function from the caret package

# Normalizing the dataset 

Train.norm.df <- Train_KNN
Valid.norm.df <- Valid_KNN
employees.norm.df <- employees


norm.values <- preProcess(Train_KNN[, 2:12], method=c("center", "scale"))

Train.norm.df[, 2:12] <- predict(norm.values, Train_KNN[, 2:12])
Valid.norm.df[, 2:12] <- predict(norm.values, Valid_KNN[, 2:12])
employees.norm.df[, 2:12] <- predict(norm.values, employees[, 2:12])

new.norm.df <- predict(norm.values, Emma)


# Task 8: Using the knn() function from the FNN package, and using a k-value of 7, generate a predicted classification for your employee.

KNN <- knn(train = Train.norm.df[, 2:12], test = new.norm.df, 
          cl = Train.norm.df[, 1], k = 7)

KNN

 row.names(Train_KNN)[attr(KNN ,'nn.index')]

neighbours <- Train_KNN[c(541, 429, 30, 665, 292, 180),]
neighbours


# Task 9: Use your validation set to help you determine an optimal k-value

accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))

for(i in 1:20) {
  knn.pred <- knn(Train.norm.df[, 2:12], Valid.norm.df[, 2:12], 
                  cl = Train.norm.df[, 1], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, Valid.norm.df[, 1])$overall[1] 
}

accuracy.df

# Task 10: Using either the base graphics package or ggplot, make a scatterplot with the various k values that you used on your x-axis, and the accuracy metrics on the y-axis.


ggplot(accuracy.df, aes(x = k , y = accuracy)) + geom_point()

# Task 11: Re-run your knn() function with the optimal k-value that you found previously

KNN <- knn(train = Train.norm.df[, 2:12], test = new.norm.df, 
           cl = Train.norm.df[, 1], k = 8)

KNN

neighbours_new <- Train_KNN[c(541, 429, 30, 665, 292, 180, 705),]
neighbours_new

## Part 2: Naive Bayes 

# Task 1: Read the file. 

emp_category <- read.csv("/Users/shimonyagrawal/Desktop/Grad /Summer 2/AD699_Data Mining/RStudio/Assignment 3/employee-categories.csv")

# Task 2: Run the str() function to check the data type for the variables in this dataframe.For any variables that are not currently factors, convert them into factors.

str(emp_category)

emp_category <- emp_category %>%
  mutate_if(is.character, as.factor)

str(emp_category)

# Task 3: Choose any three predictor variables from the dataset. For the three that you chose, make a barplot for each one. Each barplot should show one of your chosen categories on the x-axis, with ​Attrition as the fill variable. You should build proportional barplots (you can achieve this by adding position=”fill” inside your geom layer). You should generate three separate barplots for this step.

# Department, Marital Status, WorkLifeBalance

ggplot(emp_category, aes(x = Department, fill = Attrition)) + 
  geom_bar(position = 'fill') + 
  ggtitle("Impact of Department on Attrition") + 
  xlab('Department') + 
  ylab('Attrition') 

ggplot(emp_category, aes(x = MaritalStatus, fill = Attrition)) + 
  geom_bar(position = 'fill') + 
  ggtitle("Impact of Marital Status on Attrition") + 
  xlab('Marital Status') + 
  ylab('Attrition') 

ggplot(emp_category, aes(x = WorkLifeBalance, fill = Attrition)) + 
  geom_bar(position = 'fill') + 
  ggtitle("Impact of Work Life Balance on Attrition") + 
  xlab('Work Life Balance') + 
  ylab('Attrition') 

# Task 4: Using your seed value (the same one from Assignment #2) , partition your data into training (60%) and validation (40%) sets.

nrow(emp_category) *.60
emp_category_sample <- sample_n(emp_category, 1470)
Train_NB <- slice(emp_category_sample, 1:882)
Valid_NB <- slice (emp_category_sample, 883:1470)

# Task 5: Build a naive bayes model, with the response variable Attrition. Use all of the other variables in your training set as inputs.

empcat_NB <- naiveBayes(Attrition ~ ., data = Train_NB)
empcat_NB

# Task 6: Show a confusion matrix that compares the performance of your model against the training data, and another that shows its performance against the validation data. 

# Training data 
pred.class1 <- predict(empcat_NB, newdata = Train_NB) 
confusionMatrix(pred.class1, Train_NB$Attrition)

# Validation data
pred.class2 <- predict(empcat_NB, newdata = Valid_NB) 
confusionMatrix(pred.class2, Valid_NB$Attrition)

# Task 7: If you had used the naive rule as an approach to classification, how would you have classified all the records in your training set? 

# Task 8a: Emily

# Task 8b: Create a new dataframe for your person that includes his/her category values.

Emily <- data.frame(BusinessTravel = 'Travel_Frequently' , 
                   Department = 'Research & Development', 
                   Education = 'Master', 
                   EducationField = 'Technical Degree' , 
                   Gender = 'Female' ,
                   JobSatisfaction = 'High', 
                   MaritalStatus = 'Single' , 
                   PerformanceRating = 'Good ', 
                   WorkLifeBalance = 'Good')

# Task 8c: Use the predict() function in R to predict whether your person will leave the company. 

pred <- predict (empcat_NB, Emily)
pred

# Task 8d: Use the predict() function in R in a slightly different way to determine the probability that your person will leave the company. 

pred.prob <- predict(empcat_NB, newdata = Emily, type = "raw") ## predict class membership
pred.class <- predict(empcat_NB, newdata = Emily)

pred.prob
pred.class

# Task 8e: Generate a leave_score (Attrition will be Yes) and a stay_score (Attrition will be No).

# Fictional Instance: What is the leave_score and stay_score for a male employee with a marketing degree who travels frequently, works in sales and has a very high job statisfaction? 
# A-priori Probability-  No: 0.8367347 Yes: 0.1632653 
# Gender - No: 0.5921409  Yes: 0.6388889
# Business Travel- No: 0.16260163 Yes: 0.27777778 
# Marketing- No: 0.10298103 Yes: 0.14583333
# Sales- No: 0.30352304 Yes: 0.34027778 
# JobSatisfaction- No: 0.3238482 Yes: 0.1805556 

stay_score <- 0.8367347 * 0.5921409 * 0.16260163 * 0.10298103 * 0.30352304 * 0.3238482
leave_score <- 0.1632653 * 0.6388889 * 0.27777778 * 0.14583333 * 0.34027778 * 0.1805556 

stay_score / (stay_score + leave_score) # 0.758 ~ 76% probability that the employee is likely stay 
leave_score / (stay_score + leave_score) # 0.241 ~ 24% probability that the employee is likely to leave 