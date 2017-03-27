#Excersises p. 166
#1
g = 9.8
h0 = 56.67
v0  = 0
n = 25
tt = seq(0,3.4,len=n)
y = h0 + v0*tt - 0.5*g*tt^2 + rnorm(n, sd=1)
X = cbind(1, tt, tt^2)
A = solve(crossprod(X))%*%t(X)

#Use example from Matrix Algebra chapter to find LSE
LSE <- solve(crossprod(X))%*%crossprod(X,y)
LSE
#[,1]
#56.3634409
#tt  0.2808667
#-4.9596909

#Test options
LSE2 <- A%*%y
LSE2
#[,1]
#56.3634409
#tt  0.2808667
#-4.9596909

#B) A%*%y is correct.

#2 Generate 100,000 Monte Carlo simulated datasets and compute the standard error
g.list <- vector()
g.err.est <- function(num)
{ n = 25
  tt = seq(0,3.4, len=n)
  X <- cbind(1, tt, tt^2)
  for (i in 1:num){
    y <- h0 + v0*tt - 0.5*g*tt^2 + rnorm(n, sd=1)
    betahat <- solve(crossprod(X))%*%crossprod(X,y)
    g.list[i] = -2*betahat[3]}
  err <- sd(g.list)/sqrt(num)
  return(err)
  }
g.err.est(100000)
#[1] 0.001352425

#3 Generate 10,000 Monte Carlo simulated datasets and calculate the standard error
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)

height.sample <- function(N){
    index = sample(n, N)
    sampledat = father.son[index,]
    x = sampledat$fheight
    y = sampledat$sheight
    betahat = lm(y~x)$coef
    return(betahat)
}
MC <- replicate(10000, height.sample(50))
sd(MC)/sqrt(10000)
#[1] 0.1778756

#4 What is closest to the covariance between father and son heights?
n <- 100 #x and y set from previous question
mean((y - mean(y))*(x - mean(x)))
#[1] 3.869739
#C) 4 is closest

#Excercises p. 177
#1 Which will produce a design matrix for the effect of condition, controlling for days?
#A) ~day + condition

#Excercises p. 183
#1 What is the element in the first row and column of t(X)%*%X?
X <- cbind(rep(1,5 + 7), rep(c(0,1),c(5, 7)))
X.t <- t(X)%*%X
X.t[1,1]
#[1] 12

#2 What are the other entries of t(X)%*%X?
X.t
#[,1] [,2]
#[1,]   12    7
#[2,]    7    7
#The other entries are all 7.

#Excercises p. 193
library(UsingR)
n <- nrow(father.son)
N <- 50
set.seed(1)
index <- sample(n,N)
sampledat <- father.son[index,]
x <- sampledat$fheight
y <- sampledat$sheight
betahat <- lm(y~x)$coef

#1 Calculate the sum of squared residuals
fit <- lm(y~x)
Yhat <- fit$fitted.values
resid <- vector()
for (i in 1:length(y)){
  resid[i] <- (y[i] - Yhat[i])^2}
sum(resid)
#[1] 368.8264

#2 Estimate variance
sumsq <- sum(resid)
sigsq <- sumsq/(50-2)
sigsq
#[1] 7.683883

#3 Form design matrix
N <- 50
X <- cbind(rep(1,N),x)
X.solve <- solve(t(X)%*%X)
X.solve[1,1]
#[1] 13.15531

#4 Calculate standard error for the slope
diagonals <- diag(X.solve)
betahat.var <- sigsq*diagonals
s <- sqrt(betahat.var)
s
#                  x 
#10.054048  0.149665 
#The SE for the slope is 0.15

summary(fit) #Compare to second column
#Call:
#  lm(formula = y ~ x)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.9525 -1.2420  0.2626  1.5207  8.1072 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)  28.1958    10.0540   2.804 0.007255
#x             0.5981     0.1497   3.996 0.000221
#
#(Intercept) ** 
#  x           ***
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.772 on 48 degrees of freedom
#Multiple R-squared:  0.2496,	Adjusted R-squared:  0.234 
#F-statistic: 15.97 on 1 and 48 DF,  p-value: 0.0002208
