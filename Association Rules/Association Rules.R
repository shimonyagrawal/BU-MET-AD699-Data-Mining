install.packages('arules')
install.packages('arulesViz')

library(DBI)
library(odbc)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)

# Load the data set
data(Groceries)
str(Groceries)
dim(Groceries)
  
# Data Visualization 
# Generate an item frequency barplot for the Top 20 grocery item.

itemFrequencyPlot(Groceries,topN=20,type="absolute")

# Generate an item frequency barplot for the grocery items with support rate greater than 5%.

support = 0.05
itemsets <- apriori(Groceries, 
                    parameter = list(target = "frequent itemsets", supp = support, conf = 0.60, minlen = 2),
                    control = list (verbose = FALSE))

par(mar = c(5,14,2,2)+.1)
order_sets <- DATAFRAME(sort(itemsets, by = "support", decreasing = F))
barplot(order_sets$support, names.arg = order_sets$items, 
        horiz = T, las = 2, cex.names = .8, main = "Frequent Itemsets")
mtext(paste("support:", support), padj = .8)

# Generate an item frequency barplot for the grocery items with support rate greater than 3%.

support = 0.03
itemsets <- apriori(Groceries, 
                    parameter = list(target = "frequent itemsets", supp = support, conf = 0.60, minlen = 2),
                    control = list (verbose = FALSE))

par(mar = c(5,14,2,2)+.1)
order_sets <- DATAFRAME(sort(itemsets, by = "support", decreasing = F))
barplot(order_sets$support, names.arg = order_sets$items, 
        horiz = T, las = 2, cex.names = .8, main = "Frequent Itemsets")
mtext(paste("support:", support), padj = .8)

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 20  rules
arules::inspect(rules[1:20])
summary(rules)

#  Sorting out rules
rules<-sort(rules, by="confidence", decreasing=TRUE)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8, maxlen=3))

summary(rules)
arules::inspect(rules[1:20])

# Targeting Items from the Top 20 Frequency Bar Plot 

# RHS rules
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="soda"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
arules::inspect(rules[1:10])
toprules_soda <- (rules[1:10])
plot(toprules_soda, method = 'graph', engine = 'htmlwidget')



rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
arules::inspect(rules[1:10])
toprules_milk <- (rules[1:10])
plot(toprules_milk, method = 'graph', engine = 'htmlwidget')


# LHS Rules
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="root vegetables"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
arules::inspect(rules[1:5])
toprules_vegetables <- (rules[1:5])
plot(toprules_vegetables, method = 'graph', engine = 'htmlwidget')


rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="tropical fruit"),
               control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by="confidence")
arules::inspect(rules[1:5])
toprules_fruit <- (rules[1:5])
plot(toprules_fruit, method = 'graph', engine = 'htmlwidget')

# Plot a scatter plot and graph for rules 
plot(rules, method = 'graph', engine = 'htmlwidget')
plot(rules, jitter = 0)

