data {
    int<lower=1>T;
    real y[T]; 
}

parameters {
    real mu1;
    real mu2;
    real<lower=0> sigma;
    real <lower=-1,upper=1> rho;
}

transformed parameters {
      vector[T] log_p;
      vector[T] x;
      vector[T] dummy;
      real mu;
      log_p = rep_vector(-log(T), T);

      for (tau in 1:T){
        for (i in 1:T){
          if (i == 1){
            x[1] = 0;
            dummy[1] = 1;
            log_p[tau] = log_p[tau] + normal_lpdf(y[1] | mu1, sigma);
            }else{
              dummy[i] = int_step(tau-i);
              x[i] = dummy[i] * mu1 + (1 - dummy[i]) * mu2;
              mu = i < tau ? x[i] : (x[i]+rho * (y[i - 1] - x[i - 1]));
              log_p[tau] = log_p[tau] + normal_lpdf(y[i] | mu, sigma);
              }
        }
    }
}



model {
    mu1 ~ normal(0,100);
    mu2 ~ normal(0,100);
    rho ~ uniform(-1,1);
    sigma ~gamma(1,1);
    target += log_sum_exp(log_p);

} 

//Draw the discrete parameter tau. This is highly inefficient
generated quantities {
    int<lower=1,upper=T> tau;
    real es;
    tau = categorical_rng(softmax(log_p))-1;
    es = (mu2-mu1)/sigma;
}
