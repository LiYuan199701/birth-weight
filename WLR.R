##########################################################################
##	STAT6345
##	WLS
##########################################################################
setwd("C:/Users/Jing/Desktop/STAT6345/Data")
library(ggplot2)

##############   Known weights example  ##############
statin <- read.delim("statin.txt")
cor(statin$LDL, statin$Statin)

ggplot(statin, aes(Statin, LDL, size=n)) +
  geom_point(alpha=0.5)


ols.fit = lm(LDL~Statin,statin)
plot(ols.fit$fitted.values, ols.fit$residuals, pch=16)

## weighted correlation
cov.wt(cbind(statin$LDL, statin$Statin), wt=statin$n, cor=T)$cor

wls.fit = lm(LDL~Statin,statin,weights=n)
summary(wls.fit)


############## Hand speed data ##############
handspeed <- read.delim("handspeed.txt")
attach(handspeed)
plot(Age,Time,col="slateblue",pch=19)
ind <- order(Age)

## Linear fit
fit <- lm(Time~Age,handspeed)
summary(fit)

## WLS fit
fit.w <- lm(Time~Age,handspeed)
for (i in 1:20)
{
  w <- 1/fit.w$fitted.values^2
  fit.w <- lm(Time~Age,handspeed,weights=w)
}

with(handspeed,plot(Time~Age,handspeed,pch=19,col="gray"))
lines(handspeed$Age[ind],fit$fitted.values[ind],lwd=3,col=1)
lines(handspeed$Age[ind],fit.w$fitted.values[ind],lwd=3,col=2)
legend("topleft", c("OLS", "WLS"), col=c(1,2), lty=1,lwd=3)

## Bad idea
fit.w2 <- lm(Time~Age,handspeed)
for (i in 1:20)
{
  w <- fit.w2$fitted.values^2
  fit.w2 <- lm(Time~Age,handspeed,weights=w)
}
summary(fit.w2)

with(handspeed,plot(Time~Age,handspeed,pch=19,col="gray"))
lines(handspeed$Age[ind],fit$fitted.values[ind],lwd=3,col=1)
lines(handspeed$Age[ind],fit.w2$fitted.values[ind],lwd=3,col=2)
legend("topleft", c("OLS", "WLS (bad weights)"), col=c(1,2), lty=1,lwd=3)

## Quadratic fit
fit.w3 <- lm(Time~Age+I(Age^2),handspeed)
for (i in 1:20)
{
  w <- 1/fit.w3$fitted.values^2
  fit.w3 <- lm(Time~Age+I(Age^2),handspeed,weights=w)
}
fit <- lm(Time~Age+I(Age^2),handspeed)
with(handspeed,plot(Time~Age,handspeed,pch=19,col="gray"))
lines(handspeed$Age[ind],fit$fitted.values[ind],lwd=3,col=1)
lines(handspeed$Age[ind],fit.w3$fitted.values[ind],lwd=3,col=2)
legend("topleft", c("OLS", "WLS"), col=c(1,2), lty=1,lwd=3)

