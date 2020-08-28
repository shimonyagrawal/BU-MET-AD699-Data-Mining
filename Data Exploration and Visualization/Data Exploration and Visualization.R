# Individual Assignment 1: Data Exploration and Visualization by Shimony Agrawal 
# Borough and Year Assigned: Manhattan, FY 2015

# Install packages required for the data analysis
install.packages("DBI")
install.packages("odbc")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(DBI)
library(odbc)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

# Task 1: Download the file ‘nycpay.csv’ from our class Blackboard site. This dataset contains salary information for every New York City municipal employee for the fiscal years 2014-2019.

NYC_Pay = read.csv("/Users/shimonyagrawal/Desktop/Grad /Summer 2/AD699_Data Mining/RStudio/nyc_pay.csv")

# Task 2 (a): How many rows and how many columns does your dataframe contain?

dim(NYC_Pay)  

# Task 3: Filter the dataframe. Create a new object that only contains data for your assigned year and borough. How many rows does your dataframe contain now?

NYCPay_df <- NYC_Pay %>% 
  filter(Fiscal.Year == 2015, Work.Location.Borough == 'MANHATTAN')

# Task 4 (a): A. Call the str() function on your dataset. As what data type does R see the variable Agency.Start.Date? If this variable is not seen as a date, fix this -- turn this into a “Date” data type.

str(NYCPay_df$Agency.Start.Date)

NYCPay_df$Agency.Start.Date <- as.Date(NYCPay_df$Agency.Start.Date, '%m/%d/%Y') 

str(NYCPay_df$Agency.Start.Date)

# Task 4 (b): Create a new variable in the dataframe called longevity. Longevity should be the number of days between the last date in your fiscal year, and a person’s agency start date.

NYCPay_df$Longevity <- as.Date('2015-06-30') - NYCPay_df$Agency.Start.Date

# Task 4 (c): Use the arrange() function from dplyr to sort the employees by longevity, in descending order.

NYCPay_df1 <- NYCPay_df %>% 
  filter(Agency.Start.Date != '9999-12-31') %>%
  arrange(desc(Longevity)) 
  
# Task 5: Remove the variable Payroll.Number 

NYCPay_df2 <- subset(NYCPay_df, select = -(Payroll.Number))

# Task 6: Create a feature called Annual.Pay. It should be the sum of Regular.Gross.Paid + Total.OT.Paid + Total.Other.Pay.

NYCPay_df3 <- NYCPay_df2 %>%
  mutate(Annual.Pay = (Regular.Gross.Paid + Total.OT.Paid + Total.Other.Pay))

# Task 7: Identify the 8 most common agencies in the dataframe 

Top8Agency <- as.data.frame(sort(table(NYCPay_df3$Agency.Name), decreasing = TRUE)[1:8])
  
# Task 8: Create a new dataframe that only contains data for the eight most common agencies for your year & borough

NYCPay_df4 <- filter(NYCPay_df3, Agency.Name %in% Top8Agency$Var1)
  
# Task 9: Using ggplot, create a barplot that displays the number of records for the eight most-common agencies. 

ggplot(NYCPay_df4, aes(x = Agency.Name)) + 
  geom_bar(fill = 'slategray2') + 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) + 
  ggtitle("Top 8 Agencies") +
  xlab("Names of Agencies") +
  ylab("Number of Agencies")

# Task 10: Now let’s create another barplot.Put the agency names on one axis, and put the mean salaries per agency on the other axis. 

ggplot(NYCPay_df4, aes(x = Agency.Name, y = Base.Salary)) + 
  stat_summary(fun = 'mean', geom = 'bar', fill = 'violet')+ 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) + 
  ggtitle("Mean Salary per Agency") +
  xlab("Names of Agencies") +
  ylab("Mean Salaries")

# Task 11: Create a histogram that depicts the distribution of the longevity variable for your dataset. 
Histogram <- ggplot(data = NYCPay_df4, aes(x = Longevity)) + 
  geom_histogram(binwidth = 500, fill = 'violetred', color = 'lightblue') + 
  xlim(0,20000)
Histogram

# Task 12: Build another histogram from the previous step, but this time, set the fill parameter in the aesthetics layer to the agency name.

ggplot(data = NYCPay_df4, aes(x = Longevity, fill = Agency.Name)) + 
  geom_histogram(binwidth = 800, color = 'red') +
  xlim(0,20000)
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) + 
  ggtitle("Longevity by Agency Name") +
  xlab("Longevity") +
  ylab("Count")

# Task 13: Generate a visualization that lets us separately see the entire longevity histograms for each agency. Use facet_wrap to generate 8 separate histograms within the same plot.Facet on the Agency.Name variable, and use colors of your choice for the fill and borders of the bars.

Histogram + facet_wrap(~ Agency.Name)

# Task 14: Generate a boxplot that depicts agency name on one axis, and total pay on the other.
ggplot(data=NYCPay_df4, aes(x= Agency.Name, y=Annual.Pay)) + 
  geom_boxplot(outlier.colour = 'blue') + 
  theme_classic() +
  coord_flip() + 
  ggtitle("Boxplot for Top 8 Agencies based on Annual Pay") + 
  xlab("Agency Name") + 
  ylab("Annual Pay")

# Task 15: Filter the dataframe that you are currently using so that only records with a pay basis of “Per Annum” are included. Now, re-create the boxplot that you built in the previous step.
NYCPay_df5 <- NYCPay_df4 %>%
  filter(Pay.Basis == 'per Annum')

ggplot(data=NYCPay_df5, aes(x= Agency.Name, y=Annual.Pay)) + 
  geom_boxplot(outlier.colour = 'blue') + 
  theme_classic() +
  coord_flip() + 
  ggtitle("Boxplot for Top 8 Agencies based on Annual Pay") + 
  xlab("Agency Name") + 
  ylab("Annual Pay")

table(NYCPay_df4$Pay.Basis, NYCPay_df4$Agency.Name)
