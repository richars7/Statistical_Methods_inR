---
title: "Mini-Project 1"
author: "Richa Singh"
date: "September 6, 2018"
output:
  html_document:
  df_print: paged
---
  
##Question 1
  
##a)Use the density function to analytically compute the probability that the lifetime of the satellite exceeds 15 years.

a <- 1 - (2*pexp(q=15,rate=0.1,lower.tail = TRUE, log.p = FALSE) - pexp(q=15, rate = 0.2, lower.tail = TRUE, log.p = FALSE))

##b)
##i)Simulate one draw of the block lifetimes XA and XB. Use these draws to simulate one draw of the satellite lifetime T.  

xa <- 2*rexp(1,0.1)
xb <- rexp(1,0.2)
t <- 2*rexp(1,0.1) - rexp(1,0.2)
  
##ii)Repeat the previous step 10,000 times  

p.10k <- replicate(10000,(2*rexp(1,0.1) - rexp(1,0.2)))

##iii)Make a histogram of the draws of T using ???hist??? function. Superimpose the density function given above.  

hist(p.10k, probability = T)
curve((2*dexp(x,rate = 0.1)-dexp(x,rate = 0.2)), add = T, xlab = "x", ylab = "density")

##iv)Use the saved draws to estimate E(T).

mean(p.10k)

##v)Use the saved draws to estimate the probability that the satellite lasts more than 15 years.Compare with the exact answer computed in part (a).

mean(p.10k > 15)

##vi)Repeat the above process of obtaining an estimate of E(T) and an estimate of the probability four more times

mean10k <- replicate(4,mean(p.10k))
prob10k <- replicate(4, mean(p.10k > 15))

##c)Repeat part (vi) five times using 1,000 and 100,000 Monte Carlo replications instead of 10,000.Make a table of results. Comment on what you see and provide an explanation

p.1k <- replicate(1000,(2*rexp(1,0.1) - rexp(1,0.2)))
p.100k <- replicate(100000,(2*rexp(1,0.1) - rexp(1,0.2)))

mean1k<- replicate(5,mean(p.1k))
prob1k <- replicate(5, mean(p.1k > 15))

mean100k<- replicate(5,mean(p.100k))
prob100k <- replicate(5, mean(p.100k > 15))

table.data <- rbind(
  cbind(mean(p.1k), mean(p.10k), mean(p.100k)),
  cbind(mean(mean1k),mean(mean10k),mean(mean100k)),
  cbind(mean(prob1k), mean(prob10k), mean(prob100k))
)

rownames(table.data) <- c('mean of simulations','estimate of E(T) i.e mean of mean','estimate of the Probability')
colnames(table.data) <- c('for 1k simulations','for 10k simulations','for 100k simulations')

##Question 2
##Use a Monte Carlo approach estimate the value of ?? based on 10, 000 replications
#The area of the circle would be ??r2 where r is 0.5. The area of the square is s2 where s is 1.Therefore the ratio of the area of the circle to the area of the square would be ??/4. Now
#suppose we generate random points to fall uniformly in the area of the square (x varies from 0 to 1 and y varies from 0 to 1). We can state ratio of the number of points falling inside the circle
#to the total number of the points is equal to the ratio of the area of the circle to the area of the square.
#??/4 = (number of points in the circle)/(total number of points)
#Therefore, ?? can be defined by the equation. ?? = 4 * (number of points in the circle)/(total number of points)
#Generate a process that would generate random numbers between 0 and 1 for x and y positions of 10000 points and count how many points fall with the circle, then determine an estimated version of ??

montecarloPi <- function(trials) {
  count = 0
  for(i in 1:trials) {
    if((runif(1,0,1)^2 + runif(1,0,1)^2)<1) {
      count = count + 1
    }
  }
  return((count*4)/trials)
}

montecarloPi(10000)


