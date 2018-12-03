library(readxl)
library(jpndistrict)
library(tidybayes)
library(tidyverse)
library(magrittr)
library(ggstance)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(cowplot)


fukushima<-jpndistrict::jpn_pref(pref_code="7")

Sdat_fukushima <- read_excel("content/post/Sdat/Sdat_fukushima.xlsx", sheet = "Sheet2")
dH25men<-Sdat_fukushima %>% filter(year=="H25",gappei==0,ct_name!="桧枝岐村") %>% select(ct_name,自殺者数_総数, 期待死亡_総数)
names(dH25men)<-c("ct_name","d","exp_d")
write.csv(dH25men[,1:3],"H25fukushima_suicide.csv")

fukushima<-data.frame(fukushima,dH25men)


standata<-list(d=as.numeric(fukushima$自殺者数_総数),
               eD=fukushima$期待死亡_総数,
               r=nrow(fukushima))


modelPG<-"data{
  int r;
  int d[r];
  real eD[r];
}
parameters{
  real <lower=0> theta[r];
  real <lower=0> alpha;
  real <lower=0> beta;
}

model {
  for(i in 1:r){
    d[i] ~ poisson(eD[i]*theta[i]);
    theta[i] ~ gamma(alpha,beta);
  }
  alpha ~ exponential(1);
  beta ~ gamma(0.1,1);
}

generated quantities {
  real RRmean;
  real RRvar;
  RRmean = alpha/beta;
  RRvar = alpha/(beta^2);
}

"

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

SM<-rstan::stan_model(model_code=modelPG)

parameters<-c("theta","alpha","beta","RRmean","RRvar")

fit<-rstan::sampling(SM,data=standata,pars=parameters,iter = 4000,warmup = 2000,thin=2)

ffff<-as.array(fit)

ffff



library(tidybayes)
library(rstan)
aaa<-monitor(extract(fit, permuted = FALSE, inc_warmup = TRUE))


theta_gg<-fit %>% gather_draws(theta[r],alpha,beta,RRmean)

theta_gg %>% filter(.chain != 3) %>%
ggplot(aes(x=.iteration,y=.value,group=.chain,color=as.factor(.chain)))+geom_line()+facet_wrap(~.variable)

ggplot(data = theta_gg, mapping = aes(x = mu, y = sigma, group = .chain, color = factor(.chain))) +
  geom_path(size = 0.1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())


theta_gg<-fit %>% gather_draws(theta[r],alpha,beta,RRmean) %>% filter(.chain!=3)

RRmean_gg<-fit %>% spread_draws(RRmean)
RRmean_gg$r<-61
#RRmean_gg$r<-as.factor(RRmean_gg$r)
RRmean_gg<-RRmean_gg[,c(1:3,5,4)]

names(RRmean_gg)<-names(theta_gg)
theta_gg<-data.frame(rbind(data.frame(theta_gg),data.frame(RRmean_gg)))

theta_gg$r<-as.factor(theta_gg$r)

levels(theta_gg$r)<-paste0(1:60,c(fukushima$ct_name,"県平均"))

font_B <- "IPAGothic"
ggplot(theta_gg,aes(x=theta,y=fct_reorder(as.factor(r),theta,.desc = TRUE)))+ 
  geom_eyeh(aes(x = theta), .width = c(.66,.95)) +theme_bw()+coord_flip()+
  theme(text = element_text(family = "HiraKakuPro-W3"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlim(0,5)

theta_gg$r


fit %>% spread_draws(RRvar) %>% 
  ggplot(aes(x=RRvar,y=1))+geom_halfeyeh()+theme_bw()


fit %>% spread_draws(theta[r],alpha,beta) %>% 
  do(data_frame(theta=quantile(.$theta,ppoints(100))))%>%
  ggplot(aes(x=theta))+
  geom_dotplot(binwidth=.04)+
  facet_grid(cols=vars(fct_rev(as.factor(r))),switch="y")+
  facet_title_left_horizontal()+
  scale_y_continuous(breaks=NULL)+
  ylab(NULL)

fit_stan_sum<-fit %>% gather_draws(theta[r],alpha,beta) %>% 
  median_hdi(theta) 




fukushima$smr_pg<-fit_stan_sum$theta


ggplot()+geom_sf(data=fukushima,aes(fill=smr_pg))+
  scale_fill_gradientn(colours=c('grey100', 'purple'))

fit %>%
  spread_draws(theta[r], alpha,beta) %>%
  mutate(prediction = rgamma(n(), alpha, beta)) %>%
  ggplot(aes(y = r)) +
  
  # posterior predictive intervals
  stat_intervalh(aes(x = prediction), .width = c(.5, .8, .95)) +
  scale_color_brewer() +
  
  # median and quantile intervals of condition mean
  stat_pointintervalh(aes(x = theta), .width = c(.66, .95), position = position_nudge(y = -0.2)) +
  
  # data
  geom_point(aes(x = d/eD), data = Fukushima_city_suicide)


Fukushima_city_suicide$r<-1:nrow(Fukushima_city_suicide)


ggplot(fukushima,aes(geometry = geometry,fill=as.numeric(d)/as.numeric(exp_d))) +
  geom_sf(col = "white" )+
  theme_bw()

