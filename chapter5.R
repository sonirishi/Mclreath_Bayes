rm(list=ls(all=T))

library(rethinking)
library(dplyr)

data("WaffleDivorce")

d <- WaffleDivorce

d$marriage <- scale(d$Marriage)

d$medianage <- scale(d$MedianAgeMarriage)

div <- map(
  alist(
    Divorce ~ dnorm(mu,sigma),
    mu <- a + b*marriage + c*medianage,
    a ~  dnorm(10,10),
    b ~ dnorm(0,1),
    c ~ dnorm(0,1),
    sigma ~dunif(0,10)
  ),data = d
)

precis(div)

m5 <- map(
  alist(
    marriage~ dnorm(mu,sigma),
    mu <- a + c*medianage,
    a ~  dnorm(10,10),
    c ~ dnorm(0,1),
    sigma ~dunif(0,10)
  ),data = d
)

mu <- coef(m5)['a'] + coef(m5)['c']*d$medianage

resid.m <- d$marriage - mu

## Test

d <- cbind(d,resid.m)

lm(Divorce ~ resid.m,data=d)

data("milk")

d <- milk

m6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu,sigma),
    mu <- a + b*neocortex.perc,
    a ~  dnorm(10,10),
    c ~ dnorm(0,1),
    sigma ~dunif(0,10)
  ),data = d
)

dcc <- d[complete.cases(d),]

m6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu,sigma),
    mu <- a + b*neocortex.perc,
    a ~  dnorm(0,1),
    c ~ dnorm(0,0.1),
    sigma ~dunif(0,1)
  ),data = dcc
)


