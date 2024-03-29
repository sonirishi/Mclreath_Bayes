rm(list=ls(all=T))

library(rethinking)
library(dplyr)

random_walk <- replicate(1000,sum(runif(16,-1,1)))

plot(density(random_walk))

hist(random_walk)

mults <- replicate(10000,prod(1 + runif(12,0,0.1)))

plot(density(mults))

logmults <- replicate(10000,log(prod(1 + runif(12,0,0.1))))  ## log of big no to convert into small

plot(density(logmults))

data("Howell1")

d <-Howell1

d2 <- d[d$age > 18,]

plot(density(d2$height))

curve(dnorm(x,178,20),100,250)

curve(dunif(x,0,50),-10,60)

mu.list <- seq(120,200,length.out = 1000)

sigma.list <- seq(1,50,length.out = 1000)

post <- expand.grid(mu.list,sigma.list)

colnames(post) <- c("mu","sigma")

LL <- function(mu,sigma){
  sum(dnorm(d2$height,mu,sigma,log=T))
}

post$LL <- mapply(LL,post$mu,post$sigma)

post$prod <- post$LL + dnorm(post$mu,178,20,log=T) + dunif(post$sigma,0,50,log=T)

post$prob <- exp(post$prod - max(post$prod))

rows_sample <- sample(1:nrow(post),10000,replace = T,prob = post$prob)

sample.mu <- post$mu[rows_sample]

sample.sigma <- post$sigma[rows_sample]

plot(sample.mu,sample.sigma)

contour_xyz(post$mu,post$sigma,post$prob)

image_xyz(post$mu,post$sigma,post$prob)

###

logsigma <- map(
  alist(
    height ~ dnorm(mu,exp(log.sigma)),
    mu ~ dnorm(178,20),
    log.sigma ~ dnorm(2,10)
  ),data = d2
)

post <- extract.samples(logsigma)

pred_reg <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <-  a + b*weight,
    a ~ dnorm(178,20),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ),data = d2
)

alpha.list <- seq(120,200,length.out = 50)

beta.list <- seq(-10,10,length.out = 50)

sigma.list <- seq(1,50,length.out = 50)

post <- expand.grid(alpha.list,beta.list,sigma.list)

colnames(post) <- c("alpha","beta","sigma")

post$LL <- sapply(1:nrow(post),function(i){sum(dnorm(d2$height,post$sigma[i]+
                                                       d2$weight*post$beta[i],post$sigma[i],log=T))})

post$prod <- post$LL + dnorm(post$alpha,178,20,log=T) + dnorm(post$beta,0,10,log=T) + 
  dunif(post$sigma,0,50,log=T)

post$prob <- exp(post$prod - max(post$prod))

###

map.1 <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*weight,
    a ~ dnorm(178,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data = d2
)

post <- extract.samples(map.1,1000)

weight <- seq(25,75,by=1)

# grid <- expand.grid(post$a,post$b,weight)  ## this is wrong as a and b are joint distributions

calculate_mu <- function(a,b,w){
  return(a+b*w)
}

### Good function to simulate from posterior

mu.value <- sapply(weight, function(x){mapply(calculate_mu,post$a,post$b,x)}) 

dens(mu.value)

grid1 <- cbind(grid,mu.value)

plot(y=grid1[,4],x=grid[,3])

mu <- link(map.1,data=data.frame(weight))

calculate_height <- function(a,b,s,w){
  #print(a+b*w)
  return(rnorm(1,a+b*w,s))
}

height.value <- sapply(weight, function(x){mapply(calculate_height,post$a,post$b,post$sigma,x)})

height.mean <- apply(height.value,2,mean)

plot(height.mean,weight)

# Q1

weight_data <- c(46.95,43.72,64.78,32.59,54.63)

expected_height <- sapply(weight_data, function(x){mapply(calculate_height,post$a,post$b,post$sigma,x)})

height_mean <- apply(expected_height,2,mean)

height_hdpi <- apply(expected_height,2,HPDI)

d3 <- d[d$age < 18,]

height_child <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*weight,
    a ~ dnorm(100,50),
    b ~ dnorm(0,20),
    sigma ~ dunif(0,50)
  ), data = d3
)

post <- extract.samples(height_child)

weight_simulate <- seq(5,43,by=1)

mu.value <- sapply(weight_simulate, function(x){mapply(calculate_mu,post$a,post$b,x)}) 

expec.hgt <- sapply(weight_simulate, function(x){mapply(calculate_height,post$a,post$b,post$sigma,x)})

mu.value.hdpi <- apply(mu.value,2,HPDI)

expec.hgt.hdpi <- apply(expec.hgt,2,HPDI)

plot(y=d3$height, x=d3$weight)
abline(coef(height_child)[1],coef(height_child)[2])
shade(mu.value.hdpi,weight_simulate)
shade(expec.hgt.hdpi,weight_simulate,col = col.alpha("green",0.15))

## Q

full_model <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*log(weight),
    a ~ dnorm(178,100),
    b ~ dnorm(0,100),
    sigma ~ dunif(0,50)
  ), data = d
)

coef(full_model)

plot(d$height,d$weight)

plot(d$height,log(d$weight))  ### This seems more linear

post <- extract.samples(full_model)

weight_simulate <- seq(5,178,by=1)

calculate_mu_log <- function(a,b,w){
  return(a+b*log(w))
}

calculate_height_log <- function(a,b,s,w){
  #print(a+b*w)
  return(rnorm(1,a+b*log(w),s))
}

mu.value <- sapply(weight_simulate, function(x){mapply(calculate_mu_log,post$a,post$b,x)}) 

expec.hgt <- sapply(weight_simulate, function(x){mapply(calculate_height_log,post$a,post$b,post$sigma,x)})

mu.value.hdpi <- apply(mu.value,2,HPDI,0.97)

mu.value.mean <- apply(mu.value,2,mean)

expec.hgt.hdpi <- apply(expec.hgt,2,HPDI,0.97)

plot(y=d$height, x=d$weight)
lines(x=weight_simulate,y=mu.value.mean)
shade(mu.value.hdpi,weight_simulate)
shade(expec.hgt.hdpi,weight_simulate,col = col.alpha("green",0.15))
