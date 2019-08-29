library(rethinking)
library(dplyr)
library(data.table)

p_grid <- seq(0,1,length.out = 10000)

prior <- rep(1,10000)

likelihood <- dbinom(6,9,prob = p_grid)

posterior <- prior * likelihood/sum(prior * likelihood)

sample_posterior <- sample(p_grid, prob = posterior, size = 10000, replace = T)

plot(sample_posterior)

dens(sample_posterior)

print(sum(posterior[which(p_grid < 0.5)]))

print(length(which(sample_posterior < 0.5))/10000)

###

p_grid <- seq(0,1,length.out = 10000)

prior <- rep(1,10000)

likelihood <- dbinom(3,3,prob = p_grid)

posterior <- prior * likelihood/sum(prior * likelihood)

sample_posterior <- sample(p_grid, prob = posterior, size = 10000, replace = T)

PI(sample_posterior,prob=0.5)

HPDI(sample_posterior,prob=0.5)

###

w <- rbinom(10000,9,prob = sample_posterior)  ## propagating parameter uncertainty in posterior sampling

hist(w)

w_freq <- rbinom(10000,9,prob=0.6)

hist(w_freq, xlim=c(0,10), col="red")
hist(w, add=T, col=rgb(0, 1, 0, 0.5) )

data(homeworkch3)

set.seed(1234)

size <- length(birth1) + length(birth2)

boys <- sum(birth1) + sum(birth2)

init <- seq(0,1,length.out = 1000)

likelihood <- dbinom(boys,size,prob=init)

prior <- rep(1,1000)

posterior <- likelihood*prior/sum(likelihood*prior)

p_grid[which.max(posterior)]

samples <- sample(p_grid,size = 10000,prob = posterior,replace = T)

PI(samples,prob = 0.5)

rethinking::HPDI(samples,prob = 0.89)

rethinking::HPDI(samples,prob = 0.97)

mean(samples)

median(samples)

new_sample <- rbinom(10000,200,samples)

plot(density(new_sample))

quantile(new_sample)

sum(birth1)

fb_only <- rbinom(10000,100,samples)

plot(density(fb_only))

quantile(fb_only)

cnt_boys <- function(a,b){  ## Bad Code
  if(a==0){
    return(b)
  }else{
    return(0)
  }
}

cnt <- sum(mapply(cnt_boys,birth1,birth2))

print(cnt/(100-sum(birth1)))

quantile(fb_only)
