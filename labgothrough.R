library(ISLR)
attach(Wage)
head(Wage)
fit <- lm(wage ~ poly(age, 4), data = Wage)#orthogonal polynomials 
coef(summary(fit))

fit2 <- lm(wage ~ poly(age, 4,raw = TRUE), data = Wage)#raw original polynomials
coef(summary(fit2))

fit2a<- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)#Isolate the effect inside
coef(summary(fit2a))

fit2b=lm(wage ∼ cbind(age,age^2,age^3,age^4),data=Wage)#create a matrix as the same effect of I()
coef(fit2b)

#Calculate the confidence interval
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
#Is there any difference between two models' predictions?
preds2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)
max(abs(preds$fit - preds2$fit))


#Finally, we plot the data and add the fit from the degree-4 polynomial.
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age,wage,xlim=agelims ,cex=.5,col="darkgrey ")
title("Degree -4 Polynomial ",outer=T)

lines(age.grid,preds$fit ,lwd=2,col="blue")
matlines (age.grid,se.bands ,lwd=1,col="blue",lty=3)
#draws the fitted line as a solid line and the confidence bands as a
# hashed line
fit.1=lm(wage ∼ age,data=Wage)
fit.2=lm(wage ∼ poly(age ,2),data=Wage)
fit.3=lm(wage ∼ poly(age ,3),data=Wage)
fit.4=lm(wage ∼ poly(age ,4),data=Wage)
fit.5=lm(wage ∼ poly(age ,5),data=Wage)
# in order to test the nullm analysis of variance hypothesis that a model M 1 is sufficient to explain the data against the alternative hypothesis
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

fit=glm(I(wage >250) ∼ poly(age ,4),data=Wage,family=binomial )
preds=predict(fit,newdata=list(age=age.grid),se=T)

pfit=exp(preds$fit )/(1+exp(preds$fit ))
se.bands.logit = cbind(preds$fit +2*preds$se.fit , preds$fit -2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)
plot(age,I(wage >250),xlim=agelims ,type="n",ylim=c(0,.2))
points(jitter(age), I(wage >250)/5,cex=.5,pch="|",col="darkgrey ")
lines(age.grid,pfit,lwd=2, col="blue")
matlines (age.grid,se.bands ,lwd=1,col="blue",lty=3)






