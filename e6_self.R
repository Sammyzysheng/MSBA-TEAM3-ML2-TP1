## (a)Perform polynomial regression to predict wage using age . Use cross-validation to select the optimal degree d for the polynomial. What degree was chosen, and how does this compare to
#the results of hypothesis testing using ANOVA? Make a plot of the resulting polynomial fit to the data.
library(ISLR)
library(boot)
degree=seq(1,10)
head(Wage)
cv_error<-c()
for (i in degree){
  fit=glm(wage ~ poly(age,i),data=Wage)
  cv_error[i]=cv.glm(Wage,fit,K=10)$delta[1]
}
plot(degree,cv_error,type='l')
points(degree[which.min(cv_error)],min(cv_error),cex = 2,col='red',pch=19)
