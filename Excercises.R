#Falling objects
set.seed(1)
g <- 9.8
n <- 25
tt <- seq(0,3.4,len=n)
d <- 56.67 - 0.5*g*tt^2 + rnorm(n, sd=1)

library(rafalib)
mypar()
plot(tt,d,ylab="Distance in meters",xlab="Time in seconds")

#Father and son heights
library(UsingR)
x=father.son$fheight
y=father.son$sheight
plot(x,y,xlab="Father's height", ylab="Son's height")

#Random samples from multiple populations
dat <- read.csv("femaleMiceWeights.csv")
mypar(1,1)
stripchart(Bodyweight~Diet, data=dat, vertical=TRUE, method="jitter", pch=1, main="Mice weights")

#Falling object example revisited
f <- 56.67 - 0.5*g*tt^2
y <- f + rnorm(n, sd=1)
plot(tt, y, ylab="Distance in meters", xlab="Time in seconds")
lines(tt,f,col=2)

#The lm function
tt2 <- tt^2
fit <- lm(y~tt+tt2)
summary(fit)$coef

#The least squares estimate
rss <- function(Beta0,Beta1,Beta2){
  r <- y - (Beta0+Beta1*tt+Beta2*tt^2)
  return(sum(r^2))
}
Beta2s <- seq(-10,0,len=100)
plot(Beta2s,sapply(Beta2s,rss,Beta0=55,Beta1=0), ylab="RSS", xlab="Beta2", type="l")
Beta2s <- seq(-10,0,len=100)
lines(Beta2s,sapply(Beta2s,rss,Beta0=65,Beta1=0),col=2)

#Excercises p. 146

#1
data("father.son", package="UsingR")
avg.sheight <- sum(father.son$sheight)/length(father.son$sheight)
avg.sheight
#[1] 68.68407

#2
son.71 <- list()
j <- 0
for (i in 1:1078){
  if (round(father.son$fheight[i]) == 71){
    j = j+1
    son.71[j] <- father.son$sheight[i]
  }
}
mean(unlist(son.71))
#[1] 70.54082

#3
#Which cannot be written as a linear model?
#D) Y = a + bt + ct^2 + dt^3 + e
#WRONG - ANSWER IS C

#4 
#Which best describes what e represents?
#D) Between individual variability: people of the same height vary in their weight.
#MEASUREMENT ERROR

#Matrix Notation

#Vectors, Matrices and Scalars
y=father.son$fheight
head(y)
#[1] 65.04851 63.25094 64.95532 65.75250 61.13723
#[6] 63.02254

n <- 25
tt <- seq(0, 3.4, len=n)
X <- cbind(X1=tt, X2=tt^2)
head(X)
#X1         X2
#[1,] 0.0000000 0.00000000
#[2,] 0.1416667 0.02006944
#[3,] 0.2833333 0.08027778
#[4,] 0.4250000 0.18062500
#[5,] 0.5666667 0.32111111
#[6,] 0.7083333 0.50173611

dim(X)
#[1] 25  2

N <- 100; p <- 5
X <- matrix(1:(N*p),N,p)
head(X)
#[,1] [,2] [,3] [,4] [,5]
#[1,]    1  101  201  301  401
#[2,]    2  102  202  302  402
#[3,]    3  103  203  303  403
#[4,]    4  104  204  304  404
#[5,]    5  105  205  305  405
#[6,]    6  106  206  306  406

dim(X)
#[1] 100   5

X <- matrix(1:(N*p),N,p, byrow=TRUE)
head(X)
#[,1] [,2] [,3] [,4] [,5]
#[1,]    1    2    3    4    5
#[2,]    6    7    8    9   10
#[3,]   11   12   13   14   15
#[4,]   16   17   18   19   20
#[5,]   21   22   23   24   25
#[6,]   26   27   28   29   30

#Exercises p. 151

#1
X = matrix(1:1000, 100, 10)
X[25,3]
#[1] 225

#2
x=1:10
x2=2*x
x3=3*x
x4=4*x
x5=5*x
x.matrix <- cbind(x, x2, x3, x4, x5)
sum(x.matrix[7,])
#[1] 105

#3
#D) x=1:40; matrix(3*x, 20, 2)

#Matrix Operations

#Multiplying by a scalar
X <- matrix(1:12, 4, 3)
print(X)
#[,1] [,2] [,3]
#[1,]    1    5    9
#[2,]    2    6   10
#[3,]    3    7   11
#[4,]    4    8   12

a <- 2
print(a*X)
#[,1] [,2] [,3]
#[1,]    2   10   18
#[2,]    4   12   20
#[3,]    6   14   22
#[4,]    8   16   24

#The transpose
X <- matrix(1:12, 4, 3)
X
#[,1] [,2] [,3]
#[1,]    1    5    9
#[2,]    2    6   10
#[3,]    3    7   11
#[4,]    4    8   12

t(X)
#[,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]    5    6    7    8
#[3,]    9   10   11   12

#Matrix multiplication
X <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
abc <- c(3,2,1)
rbind(sum(X[1,]*abc), sum(X[2,]*abc), sum(X[3,]%*%abc))
#[,1]
#[1,]    6
#[2,]    6
#[3,]    7

X%*%abc
#[,1]
#[1,]    6
#[2,]    6
#[3,]    7

#The identity matrix
n <- 5
diag(n)
#[,1] [,2] [,3] [,4] [,5]
#[1,]    1    0    0    0    0
#[2,]    0    1    0    0    0
#[3,]    0    0    1    0    0
#[4,]    0    0    0    1    0
#[5,]    0    0    0    0    1

#The inverse
X <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
y <- matrix(c(6,2,1),3,1)
solve(X)%*%y
#[,1]
#[1,]    1
#[2,]    2
#[3,]    3

#Excersises p. 157

#1
#Which is not equivalent to a hypothetical matrix X?
#B) X %*% matrix(1,ncol(X))

#2 Solve the system of equations for c:
X <- matrix(c(3,2,1,5,4,2,-1,0,-5,2,5,0,1,-1,-5,1),4,4)
y <- matrix(c(10,5,7,4),4,1)
solve(X)%*%y
#[,1]
#[1,]  1.2477876
#[2,]  1.0176991
#[3,] -0.8849558
#[4,] -2.2389381
#c = -0.8849558

#3 Load the following matrices into R:
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
dim(a)
#[1] 4 3
dim(b)
#[1] 3 5
prod <- a%*%b
prod[3,2]
#[1] 113

#4 Element-wise vector multiplication
c <- a[3,]*b[,2]
c
#[1] 12 35 66
sum(c)
#[1] 113

#The average
y <- father.son$sheight
print(mean(y))
#[1] 68.68407

N <- length(y)
Y <- matrix(y,N,1)
A <- matrix(1,N,1)
barY = t(A)%*%Y/N
print(barY)
#       [,1]
#[1,] 68.68407

#The variance
barY=crossprod(A,Y)/N
print(barY)
#        [,1]
#[1,] 68.68407

r <- y - barY
crossprod(r)/N
#         [,1]
#[1,] 7.915196

library(rafalib)
popvar(y)
#[1] 7.915196

#Finding LSE
x=father.son$fheight
y=father.son$sheight
X <- cbind(1,x)
betahat <- solve(t(X)%*%X)%*%t(X)%*%y
betahat
#       [,1]
#33.886604
#x  0.514093

betahat2 <- solve(crossprod(X))%*%crossprod(X,y)
betahat2
#       [,1]
#33.886604
#x  0.514093

newx <- seq(min(x), max(x), len=100)
X <- cbind(1,newx)
fitted <- X%*%betahat
plot(x,y,xlab="Father's height", ylab="Son's height")
lines(newx, fitted, col=2)

#Falling object example again
set.seed(1)
g <- 9.8
n <- 25
tt <- seq(0,3.4,len=n)
d <- 56.67 - 0.5*g*tt^2 + rnorm(n, sd=1)

X <- cbind(1,tt,tt^2)
y <- d
betahat <- solve(crossprod(X))%*%crossprod(X,y)
newtt <- seq(min(tt),max(tt),len=100)
X <- cbind(1, newtt, newtt^2)
fitted <- X%*%betahat
plot(tt, y, xlab="Time", ylab="Height")
lines(newtt, fitted, col=2)
betahat
#         [,1]
#56.5317368
#tt  0.5013565
#-5.0386455

#The lm function
X <- cbind(tt, tt^2)
fit=lm(y~X)
summary(fit)
#Call:
#  lm(formula = y ~ X)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.5295 -0.4882  0.2537  0.6560  1.5455 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  56.5317     0.5451 103.701   <2e-16 ***
#  Xtt           0.5014     0.7426   0.675    0.507    
#X            -5.0386     0.2110 -23.884   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9822 on 22 degrees of freedom
#Multiple R-squared:  0.9973,	Adjusted R-squared:  0.997 
#F-statistic:  4025 on 2 and 22 DF,  p-value: < 2.2e-16

#Excercises p. 165
#1
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
X
#[,1] [,2]
#a    1    0
#a    1    0
#b    1    1
#b    1    1

beta <- c(5,2)

#What is the fitted value for the A samples?
fitA <- X%*%beta
fitA
#[,1]
#a    5
#a    5
#b    7
#b    7
#The fitted y values are 5 and 5.

#2 What is the fitted value for the B samples?
#The fitted y values are 7 and 7.

#3 
X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
X
#[,1] [,2] [,3]
#a    1    0    0
#a    1    0    0
#b    1    1    0
#b    1    1    0
#c    1    0    1
#c    1    0    1

beta <- c(10,3,-3)
X%*%beta
#[,1]
#a   10
#a   10
#b   13
#b   13
#c    7
#c    7

#The fitted value for the B samples is 13.

#4 What is the fitted value for the C samples?
#The fitted value for the C samples is 7.
