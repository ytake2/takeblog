library(rstan)
library(tidyverse)
library(tidybayes)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')


load("test/dat.Rdata")

y<-S1

ggplot(y,aes(x=time,y=outcome1))+geom_point()+geom_line()

CP_ar1<-stan_model(file="test/CP_ar1.stan")
saveRDS(CP_ar1, "CP_ar1.rds")
#fit<-sampling(model_cp1,data=list(y=y$y, T=length(y$y)))

initf1<- function(){
  list(mu1=rnorm(1, 30, 1),
       mu2=rnorm(1,40,1), 
       sigma=runif(1,0.5,2),
       rho=runif(1,-1,1),
       tau=sample(3:(T-2),1))
}

fit<-sampling(model_cp1,data=list(y=y$y, T=length(y$y)),init=initf1)

