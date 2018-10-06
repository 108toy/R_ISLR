library(tree)
library(ISLR)

attach(Carseats)

High <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

##### DD: Tree Model to predict High, using all variables but Sales
tree.carseats <- tree(High~.-Sales, Carseats)

summary(tree.carseats)
# small deviance indeicates a good fit to the (traning)