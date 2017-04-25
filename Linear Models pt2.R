#p. 194 onwwards

#Excerscises p. 224
species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))
X <- model.matrix(~species+condition)

#1
head(X)
#(Intercept) speciesB conditiontreated
#1           1        0                0
#2           1        0                1
#3           1        1                0
#4           1        1                1

library(contrast)
y <- c(1,2,3,4)
fit <- lm(y~species + condition)
contrast(fit, list(species="B", condition="control"), list(species="A", condition="treated"))$X
#(Intercept) speciesB conditiontreated
#1           0        1               -1
#attr(,"assign")
#[1] 0 1 2
#attr(,"contrasts")
#attr(,"contrasts")$species
#[1] "contr.treatment"

#attr(,"contrasts")$condition
#[1] "contr.treatment"

#Warning message:
#  In summary.lm(x) : essentially perfect fit: summary may be unreliable

#What should the contrast vector be to obtain the difference between species B control group and species A treatment group?
#D) 0 1 -1
#Note - 4 numbers given by contrast(); which 3 are important?
#Appears to represent speciesA treatment vs a speciesB control reference

#2 What is the t-statistic for the contrast of L4 vs L2?
spider <- read.csv("spider_wolff_gorb_2013.csv", skip=1)
fit <- lm(friction~type+leg, data=spider)
contrast <- contrast(fit, list(leg="L4", type="pull"), list(leg="L2", type="pull"))
#Doesn't matter whether pull or push is chosen
contrast$testStat
#1 
#2.451974 

#3 Estimate the covariance for the L4 vs L2 comparison
X <- model.matrix(~type+leg, data=spider)
Sigma.hat <- sum(fit$residuals^2)/(nrow(X)-ncol(X))*solve(t(X)%*%X)
C <- matrix(c(0,0,-1,0,1),1,5)
cov <- Sigma.hat^2*(solve(t(X)%*%X))
cov[5,3]
#[1] 6.003178e-09

#4 t-statistic for log transformed data
spider$log2friction <- log2(spider$friction)
boxplot(log2friction~type*leg, data=spider)
fit2 <- lm(log2friction~type*leg, data=spider)
summary(fit2)
#Call:
#  lm(formula = log2friction ~ type * leg, data = spider)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.35902 -0.19193  0.00596  0.16315  1.33090 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -0.16828    0.06613  -2.545 0.011487 *  
#  typepush       -1.20656    0.09352 -12.901  < 2e-16 ***
#  legL2           0.34681    0.11952   2.902 0.004014 ** 
#  legL3           0.48999    0.08505   5.762 2.24e-08 ***
#  legL4           0.64668    0.08995   7.189 6.20e-12 ***
#  typepush:legL2  0.09967    0.16903   0.590 0.555906    
#typepush:legL3 -0.54075    0.12027  -4.496 1.02e-05 ***
#  typepush:legL4 -0.46920    0.12721  -3.689 0.000272 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.3856 on 274 degrees of freedom
#Multiple R-squared:  0.8125,	Adjusted R-squared:  0.8077 
#F-statistic: 169.6 on 7 and 274 DF,  p-value: < 2.2e-16

#The t-statistic for typepush:legL4 is -3.689.

#5 What is the F-value for all of the type:leg interaction terms in an ANOVA?
anova(fit2)
#Analysis of Variance Table
#
#Response: log2friction
#Df  Sum Sq Mean Sq  F value    Pr(>F)    
#type        1 164.709 164.709 1107.714 < 2.2e-16 ***
#  leg         3   7.065   2.355   15.838 1.589e-09 ***
#  type:leg    3   4.774   1.591   10.701 1.130e-06 ***
#  Residuals 274  40.742   0.149                       
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#The F-value is 10.701.

#6 What is the L2 vs L1 estimate in log2(friction) for the pull samples?
contrast <- contrast(fit2, list(type="pull", leg="L1"), list(type="pull", leg="L2"))
contrast$X
#(Intercept) typepush legL2 legL3 legL4 typepush:legL2 typepush:legL3
#1           0        0    -1     0     0              0              0
#typepush:legL4
#1              0
#attr(,"assign")
#[1] 0 1 2 2 2 3 3 3
#attr(,"contrasts")
#attr(,"contrasts")$type
#[1] "contr.treatment"
#
#attr(,"contrasts")$leg
#[1] "contr.treatment"

#The estimate is 0.

#7 What is the L2 vs L1 estimate in log2(friction) for the push samples?
contrast <- contrast(fit2, list(type="push", leg="L1"), list(type="push", leg="L2"))
contrast$X[3]
#[1] -1

#Excersises p. 231
#1 Which design matrix does not have collinearity?
#B

#2 What is the sum of squared residuals when the male coefficient is 1 and the D coefficient is 2?
sex <- factor(rep(c("female", "male"), each=4))
trt <- factor(c("A","A","B","B","C","C","D","D"))
x <- model.matrix(~sex+trt)
qr(x)$rank
#[1] 4
Y <- 1:8
makeYstar <- function(a, b) Y - x[,2]*a-x[,5]*b
fitTheRest <- function(a,b){
  Ystar <- makeYstar(a,b)
  Xrest <- x[,-c(2,5)]
  betarest <- solve(t(Xrest)%*%Xrest)%*%t(Xrest)%*%Ystar
  residuals <- Ystar - Xrest %*% betarest
  sum(residuals^2)
}
fitTheRest(1,2)
#[1] 11

#3 What is the smallest sum of squared residuals?
grid <- outer(-2:8,-2:8,Vectorize(fitTheRest))
grid
#[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
#[1,]  102   83   66   51   38   27   18   11    6     3     2
#[2,]   83   66   51   38   27   18   11    6    3     2     3
#[3,]   66   51   38   27   18   11    6    3    2     3     6
#[4,]   51   38   27   18   11    6    3    2    3     6    11
#[5,]   38   27   18   11    6    3    2    3    6    11    18
#[6,]   27   18   11    6    3    2    3    6   11    18    27
#[7,]   18   11    6    3    2    3    6   11   18    27    38
#[8,]   11    6    3    2    3    6   11   18   27    38    51
#[9,]    6    3    2    3    6   11   18   27   38    51    66
#[10,]    3    2    3    6   11   18   27   38   51    66    83
#[11,]    2    3    6   11   18   27   38   51   66    83   102
min(grid)
#[1] 2