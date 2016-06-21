library(ISLR)
summary(Hitters)
Hitters = na.omit(Hitters)
install.packages("leaps") 
library(leaps)
regfitfull = regsubsets(Salary~. , data = Hitters)
summary(regfitfull)
regfit.full = regsubsets(Salary~. , data = Hitters,nvmax = 19)
reg.summary = summary(regfit.full)
names(reg.summary)
#using Cp
plot(reg.summary$cp, xlab = "No. of var" , ylab = "Cp Stat")
which.min(reg.summary$cp)
plot(regfit.full,scale = "Cp")
coef(regfit.full,10)
plot(reg.summary$bic, xlab = "No. of var" , ylab = "bic Stat")
which.min(reg.summary$bic)
plot(reg.summary$adjr2, xlab = "No. of var" , ylab = "adjusted R-square Stat")
coef(regfit.full,7) 
#forward and Backward Selection
regfit.fwd = regsubsets(Salary~. , data = Hitters,nvmax = 19, method = "forward")
summary(regfit.fwd)
#Similarly Backward Selection
#Validation
set.seed(1)
No.Observations = dim(Hitters)[1]
No.Observations
train = sample(No.Observations , 180 ,rep = FALSE )
test = -train
regfit.best = regsubsets(Salary ~. , data = Hitters[train,] , nvmax =  19)
test.mat = model.matrix(Salary ~. , data = Hitters[test,])
test.mat
#Validation
val.errors = rep(0,19)
for(i in 1:19){
  coefi = coef(regfit.best, id = i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((Hitters$Salary[test]- pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best , 5)
#Ridge and Lasso
#Ridge Algo
#for Ridge Alpha = 0 , and for Lasso Alpha = 0
install.packages("glmnet")
library(glmnet)
grid = 10^(seq(10,-2,length = 100))
grid
x = model.matrix(Salary~., data = Hitters)[,-1]
y = Hitters$Salary
ridge.model = glmnet(x,y,alpha = 0, lambda = grid)
dim(coef(ridge.model))
#cross validate the model
cv.out = cv.glmnet(x,y,alpha = 0)
plot(cv.out)
names(cv.out)
bestlamda = cv.out$lambda.min
bestlamda
Model.ridge = glmnet(x,y,alpha = 0)
predict(Model.ridge, type = "coefficients", s = bestlamda)[1:20,]
# Using Lasso
library(glmnet)
grid = 10^(seq(10,-2,length = 100))
grid
x = model.matrix(Salary~., data = Hitters)[,-1]
y = Hitters$Salary
ridge.model = glmnet(x,y,alpha = 1, lambda = grid)
dim(coef(ridge.model))
#cross validate the model
cv.out = cv.glmnet(x,y,alpha = 1)
plot(cv.out)
names(cv.out)
bestlamda = cv.out$lambda.min
bestlamda
Model.lasso = glmnet(x,y,alpha = 1)
predict(Model.lasso , type = "coefficients", s = bestlamda)[1:20,]