#### code for chapter 9
#### from page 373
library(e1071) # libraryis a good for SVM

library(LiblineaR) # another option. Good for large linear problems


set.seed(1)
x <- matrix(rnorm (20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
plot(x, col=(3-y))
# Not linearly separable

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y~., data = dat , kernel = "linear", cost = 10, scale = FALSE) 

# When the cost argument is small, then the margins will be wide and more support vectors will be used
# argument scale=FALSE tells the svm() function not to scale each feature to have mean zero or standard deviation one

plot(svmfit , dat)
# x are the support vectors. Here is how to see them:

svmfit$index

summary(svmfit)

#### Now try a smaller COST =

svmfit <- svm(y~., data = dat , kernel = "linear", cost = 0.1, scale = FALSE) 
plot(svmfit , dat)
svmfit$index

# tune function
tune.out <- tune(svm ,y~.,data = dat ,kernel = "linear",
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) )) # try 
summary(tune.out)

best_mod <- tune.out$best.model
summary(best_mod)

# now we predict:
xtest <- matrix(rnorm (20*2) , ncol=2)
ytest <- sample (c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]= xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest , y=as.factor(ytest))

ypred <- predict (best_mod ,testdat)
table(predict = ypred, truth = testdat$y)

# compoared to use cost of 0.01
svmfit_c001 <- svm(y~., data = dat , kernel = "linear", cost = 0.01, scale = FALSE) 

ypred_c001 <- predict (svmfit_c001 ,testdat)
table(predict = ypred_c001, truth = testdat$y)

#######

# a linearly separable
x[y==1, ] <- x[y==1, ] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

# use a very large COST, so that no misclassification is allowed:

dat = data.frame(x=x,y=as.factor(y))
svmfit = svm(y~., data=dat , kernel ="linear", cost=1e5)
summary(svmfit)

plot(svmfit , dat)


#### 9.6.2 Support Vector Machine

# to fit an SVM using a non-linear kernel

# to fit an SVM with a polynomial kernel use kernel="polynomial", se the degree argument to specify a degree for the polynomial kernel (this is d in (9.22)),
# to fit an SVM with a radial kernel we use kernel="radial".  u and in the latter case we use gamma to specify a value of γ for the radial basis kernel (9.24)

set.seed(1)
x = matrix(rnorm (200*2) , ncol = 2)
x[1:100,] = x[1:100,]+2
x[101:150 ,] = x[101:150,] - 2
y = c(rep(1,150) ,rep(2,50))
dat = data.frame(x = x,y = as.factor(y))

plot(x, col=y) # Non linear

train = sample(200,100)
svmfit = svm(y~., data = dat[train ,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit , dat[train ,])

summary(svmfit)

# tune the model:
set.seed(1)
tune.out = tune(svm , y~., data=dat[train ,], kernel ="radial",
                ranges=list(cost=c(0.1,1,10,100,1000),
                            gamma=c(0.5,1,2,3,4) ))
summary(tune.out)

table(true=dat[-train ,"y"], pred=predict (tune.out$best.model ,
                                           newdata =dat[-train ,]))


#### 9.6.3 ROC Curves

library(ROCR)
rocplot =function (pred , truth , ...){
  predob = prediction (pred , truth)
  perf = performance(predob , "tpr", "fpr")
  plot(perf ,...)
  }

# fitted value: > 0 one class; < 0 another class
svmfit.opt <- svm(y~., data=dat[train ,], kernel ="radial",
               gamma=2, cost=1, decision.values =T)

fitted <- attributes(predict(svmfit.opt ,dat[train ,], decision.values=TRUE))$decision.values

# ROC plot

par(mfrow=c(1,3))
rocplot (fitted ,dat[train ,"y"], main="Training Data")

fitted = attributes(predict(svmfit.opt ,dat[-train ,], decision.values=T))$decision.values
rocplot(fitted ,dat[-train ,"y"], main="Test Data")

fitted = attributes (predict(svmfit.flex ,dat[- train ,], decision.values=T))$decision.values
rocplot(fitted ,dat[-train ,"y"],add=T,col="red")

# 9.6.4 SVM with Multiple Classes
