data {
  int<lower=0> numObs;
  int<lower=0> numAY;
  int<lower=0> numLags;
  
  vector<lower=0>[numAY] premium;
  vector<lower=0>[numObs] loss;
  int<lower=0> AY_Index[numObs];
  int<lower=0> lag[numObs];
  
  real<lower=0> elr_lower;
  real<lower=0> elr_upper;
  
  real<lower=0> sd_alpha;
}

transformed data {
  vector[numAY] log_premium;
  vector[numObs] log_loss;
  real log_elr_lower;
  real log_elr_upper;
  
  log_premium = log(premium);
  log_loss = log(loss);
  
  log_elr_lower = log(elr_lower);
  log_elr_upper = log(elr_upper);
}

parameters {
  real elr;
  real<lower=0> sigma;
  vector[numAY] alpha;
  vector[numLags] beta;
}

transformed parameters {
  real log_elr;
  
  log_elr = log(elr);
}

model {
  vector[numObs] mu;
  
  log_elr ~ uniform(log_elr_lower, log_elr_upper);
  alpha ~ normal(log_premium + log_elr, sd_alpha);
  beta ~ uniform(-5, 5);
  
  mu = alpha[AY_Index] + beta[lag];
  
  // mu[ay, lag] = alpha[ay] + beta[lag];
  // log_loss ~ normal(mu[ay, lag], sigma[lag])
  log_loss ~ normal(mu, sigma);
 
}
