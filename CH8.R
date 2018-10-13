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
# small deviance indicates a good fit to the (traning)
# residual mean deviance reported is simply the deviance divided by n−|T0|
# n: number of samples
# |T0|: number of nodes

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
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
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

library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~., Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston , pretty = 0)

#### Cross validation

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,type='b')

# the most complex tree is the best
# but if we still wait to prune the tree we can:

prune.boston=prune.tree(tree.boston ,best=5)
plot(prune.boston)
text(prune.boston , pretty =0)

# then the prediction

yhat=predict (tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat ,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)

##### 8.3.3 Bagging and Random Forests #####
library(randomForest)
# Bagging is RF as a special case m = p

# bagging
set.seed(1)
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston

# Explanation on the function
# mtry = 13: all 13 predictors should be considered for each split of the tree, so m = p, so bagging

# then use the model to predict
yhat.bag = predict (bag.boston , newdata = Boston[-train ,])
plot(yhat.bag , boston.test)
abline (0,1)
mean((yhat.bag -boston.test)^2)

# The test set MSE associated with the bagged regression tree is 13.16, 
# almost half that obtained using an optimally-pruned single tree

##### Bagging
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag = predict(bag.boston , newdata = Boston[-train ,])
mean((yhat.bag -boston.test)^2)

##### Random Forest
set.seed(1)
rf.boston= randomForest(medv~., data=Boston, subset=train, mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)


importance(rf.boston)
# higher number, more important

# plot the importance:
varImpPlot (rf.boston)

##### Boosting
library (gbm)
set.seed(1)
# regression problem: distribution = "gaussian"
# classification problem:  distribution="bernoulli"
boost.boston = gbm(medv~., data = Boston[train ,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)

summary(boost.boston)
# rel.inf => higher, more important the factor. in this case, it is again lstat and rm
# produce partial dependence plot 
# illustrate the marginal  effect of the selected variables on the response after INTEGRATING out the other variables

par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

# Boosted model to predict

yhat.boost=predict(boost.boston ,newdata =Boston[-train ,], n.trees=5000)
mean((yhat.boost - boston.test)^2)
# MSE is similar to Random Forest and superior to Bagging

# now try different shrinkage parameter λ, default is 0.001
boost.boston = gbm(medv~., data=Boston[train ,], distribution="gaussian", n.trees =5000, interaction.depth = 4, shrinkage = 0.2, verbose=F)
yhat.boost = predict(boost.boston ,newdata =Boston[-train ,], n.trees=5000)
mean((yhat.boost - boston.test)^2) # MSE




