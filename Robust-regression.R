##########################################################################
##	STAT6345
##	Robust Regression
##########################################################################
setwd("C:/Users/Jing/Desktop/STAT6345/Data")
library(ggplot2)

require(MASS)
plot(phones$year,phones$calls,pch=19,xlab="Year",ylab="Millions of calls")
fit <- lm(calls~year,phones)
abline(fit$coef,lwd=3)
fit <- rlm(calls~year,phones,maxit=50)
abline(fit$coef,lwd=3,col="red")
fit <- rlm(calls~year,phones,psi=psi.bisquare)
abline(fit$coef,lwd=3,col="blue")
legend("topleft",legend=c("Least squares","Huber","Tukey"),lwd=3,col=c("black","red","blue"))

## Slide 7
u <- seq(-6,6,len=101)
plot(u,u^2,lwd=3,type="l",col="black",ylim=c(0,6),xlab="Residual",ylab="Loss")
lines(u,abs(u),lwd=3,col="green")
y <- numeric(length(u))
p <- function(x){psi.huber(x)*x}
for (i in 1:101) y[i] <- integrate(p,u[1],u[i])$value
y <- y-min(y)
lines(u,y,type="l",lwd=3,col="red")
p <- function(x){psi.bisquare(x)*x}
for (i in 1:101) y[i] <- integrate(p,-3.1,u[i])$value
y <- y-min(y)
lines(u,y,type="l",lwd=3,col="blue")
legend("bottomleft",col=c("black","green","red","blue"),legend=c("Least squares","Absolute value","Huber","Tukey"),lty=1,lwd=3)


####### 
load("lattice.RData")
ggplot(dating, aes(x = carbon, y = thorium - carbon)) +
  geom_point()+theme_bw()

library(tidyverse)

dating = dating %>% mutate(diff = thorium - carbon)
dating.lm = lm(diff ~ carbon, data = dating)
dating.rlm.huber = rlm(diff ~ carbon, data = dating, psi = psi.huber, maxit = 100)
dating.rlm.bisquare = rlm(diff ~ carbon, data = dating, psi = psi.bisquare)
 
ind <- order(dating$carbon)

plot(dating$carbon,dating$diff,pch=19,col="gray", ylim=c(0,7), xlab="carbon", ylab="diff")
lines(dating$carbon[ind],dating.lm$fitted.values[ind],lwd=3,col=1)
lines(dating$carbon[ind],dating.rlm.huber$fitted.values[ind],lwd=3,col=2)
lines(dating$carbon[ind],dating.rlm.bisquare$fitted.values[ind],lwd=3,col=3)
legend("topleft", c("OLS", "Huber", "Bisquare"), col=1:3, lty=1,lwd=3)

dating.rlm.0 = lm(diff ~ carbon, data = dating)
p0 = ggplot(dating) + theme_bw() +
  geom_point(aes(x = carbon, y = diff)) +
  geom_line(aes(x = carbon, y = dating.rlm.0$fitted.values), color = 'red') +
  ylim(c(0,7))+
  annotate("label", x = 10, y = 6, label = "iteration = 0", size = 3)


dating.rlm.5 = rlm(diff ~ carbon, data = dating, maxit = 5, psi = psi.bisquare)
p1 =ggplot(dating) + theme_bw() +
  geom_point(aes(x = carbon, y = diff)) +
  geom_line(aes(x = carbon, y = dating.rlm.5$fitted.values), color = 'red') +
  ylim(c(0,7))+
  annotate("label", x = 10, y = 6, label = "iteration = 5", size = 3)


dating.rlm.10 = rlm(diff ~ carbon, data = dating, maxit = 10, psi = psi.bisquare)
p2 =ggplot(dating) + theme_bw() +
  geom_point(aes(x = carbon, y = diff)) +
  geom_line(aes(x = carbon, y = dating.rlm.10$fitted.values), color = 'red') +
  ylim(c(0,7)) +
  annotate("label", x = 10, y = 6, label = "iteration = 10", size = 3)


 

dating.rlm.15 = rlm(diff ~ carbon, data = dating, maxit = 15, psi = psi.bisquare)
p3 = ggplot(dating) + theme_bw() +
  geom_point(aes(x = carbon, y = diff)) +
  geom_line(aes(x = carbon, y = dating.rlm.15$fitted.values), color = 'red') +
  ylim(c(0,7)) +
  annotate("label", x = 10, y = 6, label = "iteration = 15", size = 3)

library(patchwork)
(p0|p1)/(p2|p3)

fits = sapply(seq(1,15,by = 1), function(maxit)
  rlm(diff ~ carbon, data = dating, init = dating.lm,
      maxit = maxit, psi = psi.bisquare)$fitted.values)
fits = data.frame(fits, carbon = dating$carbon, diff = dating$diff)
fits_melted = fits %>% gather(X1:X15, key = iter_char, value = fit)
fits_final = fits_melted %>%
  separate(iter_char, sep = "X", into = c("none", "it")) %>%
  mutate(iteration = factor(it, levels = as.character(1:15), ordered = TRUE))
ggplot(fits_final) + theme_bw() +
  geom_point(aes(x = carbon, y = diff)) + 
  geom_line(aes(x = carbon, y = fit, color = iteration)) +
  theme(legend.key.size = unit(2, "mm"), legend.text=element_text(size=10))
 
## Comparison of the penalties
huber = function(e, k) {
  if(abs(e) < k) return(.5 * e^2)
  k * abs(e) - .5 * k^2
}
bisquare = function(e, k) {
  if(abs(e) < k) return (k^2 / 6 * (1 - (1 - (e/k)^2)^3))
  return(k^2 / 6)
}
evec = seq(-6, 6, length.out = 400)
huberpen = sapply(evec, huber, 1.345)
bisquarepen = sapply(evec, bisquare, 4.685)
leastsquares = evec^2
penalties = data.frame(error = evec, huber = huberpen, bisquare = bisquarepen, leastsquares)
penalties = penalties %>% gather(huber, bisquare, leastsquares, key = "type", value = "penalty")
ggplot(penalties, aes(x = error, y = penalty)) +
  geom_line() + facet_wrap(~ type, scales = "free_y")