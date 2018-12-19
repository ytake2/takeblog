#Program begins
#lines that begin with ‘#’ are comments and will not be executed
#install.packages(runjags)
#install runjags if it is not already installed
library(runjags)
#load runjags
#y2 <- c(0.845, 0.740, 0.912, 0.960, 0.847, 0.651, 0.946, 0.703, 0.995, 0.916,
#       2.048, 1.828, 1.985, 1.968, 2.241, 1.881, 2.015, 1.789, 1.868, 2.001
#)

load("dat.Rdata")
y2<-y$y

#y is a vector of 20 observations
T <- length(y2)
#Total number of time-points in the data (in this case, 20)
P <- 2
#two phases – baseline and treatment phase
pi <- c(rep(0, 2), rep(1/(T-4), (T-4)), rep(0, 2))
#probabilities for the categorical prior for change-point
#i.e. pi will equal (0, 0, 1/16, …, 1/16, 0, 0) because the change-point
#can only be between time point 3 and T-2
b1 <- mean(y2[1:(T/2)])
#mean of the distribution. This is used to generate starting values for
#intercept 1 when the model is fitted
b2 <- mean(y2[((T/2)+1):T])
#mean of the distribution. This is used to generate starting values for
#intercept 2 when the model is fitted
#model definition begins here, the model is denoted using variable called
#UCP.model
UCP.model <- "model {
 x[1] <- 0
 #x is a vector of means of distributions from which the intercepts are drawn

 yhat[1] <- beta[1, 1] 
 #beta[1, 1] is the mean of the distribution from which intercept 1 is drawn

 y[1] ~ dnorm(yhat[1], tau.epsilon)
 #y[1] is generated from normal distribution with mean yhat[1] and precision tau.epsilon, tau.epsilon is the inverse of the variance in equation 1

 for (i in 2:T) {
 dummy[i] <- step(cp - i)
 # dummy = 1 if i in the baseline phase, dummy = 0 if i in the intervention phase

 x[i] <- dummy[i] * beta[1, 1] + (1 - dummy[i]) * beta[2, 1]
 #identifies if x for the ith time-point should be centered around intercept 1 or intercept 2

 yhat[i] <- ifelse(cp == i + 1, x[i], x[i] + rho * (y[i - 1] - x[i - 1]))
 #if i is the first time-point of phase 2 no autocorrelation

 y[i] ~ dnorm(yhat[i], tau.epsilon)
 #y[i] is generated from normal distribution with mean yhat[i] and precision tau.epsilon
 }

 #Assigning priors
 for (i in 1:P){
 beta[i, 1] ~ dnorm(mu[i], prec[i])
 #intercept[i] is drawn from a normal distribution with mean mu[i] and precision prec[i]

 mu[i] ~ dnorm(0, .0001)
 # mu is drawn from a normal distribution with mean 0 and precision .0001 (i.e. variance 10000)

 prec[i] ~ dgamma(1, 1)
 #precision is drawn from a gamma distribution with shape 1
 }
 
 sigma.epsilon ~ dunif(0.1, 5) 
 # standard deviation of y within each phase is drawn from a uniform distribution ranging from 0.1 to 5
 tau.epsilon <- pow(sigma.epsilon, -2)
 #tau.epsilon is the precision of y obtained from sigma.epsilon

 cp ~ dcat(pi)
 #prior of the change-point is categorical with probabilities as defined in the pi vector on line 6

 rho ~ dunif(-1, 1)
 #autocorrelation is drawn from a uniform distribution ranging from -1 to 1
}"

#end of model definition
#run jags to estimate the parameters
#autorun.jags calls runjags to run the model in JAGS until convergence
results <- autorun.jags(
  model = UCP.model,
  #the model defined in the previous block is assigned as the model to be
  #fitted
  data = list(y = y2, T = T, P = P, pi = pi),
  #data input: observations, #time-points, #phases, #priors for change-point
  monitor = c("beta", "sigma.epsilon", "rho", "cp"),
  #parameters to monitor
  n.chains = 4,
  #run 4 chains
  startsample = 50000,
  #burn-in the first 50000
  inits = function() { #initial values are generated within this function
    list(
      beta = rbind(rnorm(1, b1, 1), rnorm(1, b2, 1)),
      sigma.epsilon = runif(1, 0.5, 2),
      rho = runif(1, -1, 1),
      cp = sample(3:(T-2), 1) 
    )
  },
  method = "rjparallel"
  #run parallel chains
)
#end of running jags
# combine all chains into a single chain for convenience
results$draws <- combine.mcmc(results$mcmc)
results #object that contains all posteriors

aaa<-results$draws

library(tidybayes)
results %>% gather_draws(rho) %>% ggplot(aes(x=.value,y=.variable))+ geom_halfeyeh()
#will produce trace, density, and other plots for all parameters 