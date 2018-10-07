##### Code for Chapter 8
library(ISLR) #### DD: data for the book

library(tree)
attach(Carseats)

High <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

##### 8.3 Lab: Decision Trees ##### 
##### 8.3.1 Fitting Classification Trees ##### 
##### DD: Tree Model to predict High, using all variables but Sales
tree.carseats <- tree(High~.-Sales, Carseats)

summary(tree.carseats)
##### DD: results
# small deviance indeicates a good fit to the (traning)
# residual mean deviance reported is simply the deviance divided by n−|T0|
# n: 

plot(tree.carseats)
text(tree.carseats)

##### DD: we must estimate the test error rather than simply computing the training error

set.seed(2)
train <- sample (1: nrow(Carseats ), 200)
Carseats.test <- Carseats [-train ,]
High.test <- High[-train]
tree.carseats <- tree(High~.-Sales , Carseats ,subset=train)

# the argument type="class" instructs R to return the actual class prediction
tree.pred <- predict(tree.carseats , Carseats.test, type="class")

#### Step 1: To see the prediction correct rate
table(tree.pred, High.test)

##### Step 2: we consider whether pruning the tree might lead to improved results

# function cv.tree() performs cross-validation in order to determine the optimal level of tree complexity; 
# cost complexity pruning is used in order to select a sequence of trees for consideration

# FUN=prune.misclass 
# in order to indicate that we want the classification error rate to guide the cross-validation and pruning process,
# rather than the default for the cv.tree() function, which is deviance

set.seed(3)
cv.carseats <- cv.tree(tree.carseats,FUN = prune.misclass)
names(cv.carseats)

# result of cv.carseats
# size: terminal nodes
# dev:  corresponding dev for each tree terminal nodes 
# in this case, the tree with 9 nodes, results in the lowest cross-validation error rate 50.

#### Plot the error arate as a function of both size and k
par(mfrow = c(1,2))

plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

#### So Nine node it is ####

prune.carseats <- prune.misclass(tree.carseats, best = 9 ) # 9 nodes tree
plot(prune.carseats)
text(prune.carseats, pretty  = 0)

#### Try the new 9 nodes tree
tree.pred <- predict(prune.carseats, Carseat.test, type = "class")
table(tree.pred, High.test)

####


##### 8.3.2 Fitting Regression Trees ##### 