data{
  int r; // 地域数
  int d[r]; //各地域の自殺発生数
  real exp_d[r];　//各地域の期待死亡数
}

parameters{
  real <lower=0> theta[r];
  real <lower=0> alpha;
  real <lower=0> beta;
}

model {
  for(i in 1:r){
    d[i] ~ poisson(exp_d[i]*theta[i]);
    theta[i] ~ gamma(alpha,beta);
  }
  alpha ~ exponential(1);
  beta ~ gamma(0.1,1);
}

generated quantities {
  real RRmean; 
  real RRvar;
  int  dPred[r];
  RRmean = alpha/beta; // 市区町村の自殺相対リスクの県平均
  RRvar = alpha/(beta^2); // 市区町村の自殺相対リスクの分散
  for(i in 1:r){
  dPred[i] = poisson_rng(exp_d[i]*theta[i]);
  }
}
