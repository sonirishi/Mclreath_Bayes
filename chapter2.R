rm(list = ls(all=T))

p_grid <- seq(0,1,length.out = 20)

prior <- rep(1,20)

likelihood <- dbinom(6,9,prob = p_grid)

posterior <- prior * likelihood/sum(prior * likelihood)

plot(p_grid,posterior,type='b')

###

p_grid <- seq(0,1,length.out = 20)

prior <- ifelse(p_grid < 0.5, 0, 1)

likelihood <- dbinom(6,9,prob = p_grid)

posterior <- prior * likelihood/sum(prior * likelihood)

plot(p_grid,posterior,type='b')

###

p_grid <- seq(0,1,length.out = 20)

prior <- exp(-5*abs(p_grid - 0.5))

likelihood <- dbinom(6,9,prob = p_grid)

posterior <- prior * likelihood/sum(prior * likelihood)

plot(p_grid,posterior,type='b')

###

library(rethinking)

globe.qa <- rethinking::map(
  alist(
    w ~ dbinom(9,p),
    p ~ dunif(0,1)
    ),
  data=list(w=6))

precis(globe.qa)


w <- 6
n <- 9
curve(dbeta(x,w+1,n-w+1),from = 0, to = 1)
curve(dnorm(x,0.67,0.16),add=T)