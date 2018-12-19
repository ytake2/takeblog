
fit %>% spread_draws(tau) %>% ggplot()+geom_histogram(aes(x=tau))


library(tidybayes)
fit %>% tidybayes::spread_draws(mu2) %>% ggplot()+geom_halfeyeh(aes(x=mu2,y=1))
fit %>% tidybayes::gather_draws(mu1,mu2,rho,es,sigma) %>% point_intervalh()

ms <- rstan::extract(fit)
q <- colMeans(exp(ms$lp))
q <- q/sum(q)

plot(q)

barplot(q)
qplot(names(q),q,geom="bar",stat="identity")

#fit %>% spread_draws(tau) %>% ggplot(aes(x=tau))+geom_histogram() 
#  mutate(exp_lp=exp(lp)) %>%  median_hdci() -> med_hd

#g1<-med_hd %>% mutate(exp_lp_ratio=exp_lp/sum(med_hd$exp_lp)) %>% 
#  ggplot(aes(x=T,y=exp_lp_ratio,group=1))+geom_bar(stat="identity")