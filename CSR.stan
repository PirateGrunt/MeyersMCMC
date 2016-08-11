data {
  // sample data
  int<lower=1> numAY;
  int<lower=1> numDevs;
  int<lower=1> numObs;
  vector[numAY] premium;

  vector[numObs] cumulative_loss;
  vector[numObs] AY_Index;
  vector[numObs] Lag;
  
  // prior parameters
  // In the paper, Meyers uses a lower bound of 1.0
  real log_elr_lower;
  real log_elr_upper;
  real gamma_mean;
  real gamma_sd;
  real alpha_sd;
  real beta_lower;
  real beta_upper;
  real a_lower;
  real a_upper;
  
  // New predicted quantities

}

transformed data {
  vector[numObs] log_loss;
  
  log_loss = log(cumulative_loss);
  
}

parameters {
  real log_elr;
  real gamma;               // gamma is a parameter, not a function here
  
  vector[numAY] alpha;
  vector[numDevs] beta;
  real sigma[numDevs];
  real aSig;
  vector[numObs] mu;
}

transformed parameters {
  real log_elr;
  
  log_elr = log(elr);
}

model {
  log_elr ~ uniform(log_elr_lower, log_elr_upper);
  gamma ~ normal(gamma_mean, gamma_sd);
  
  for (iDev in 1:(numDevs - 1)){
    aSig ~ uniform(a_lower, a_upper);
    
    if (iDev == 1)
      sigma[iDev] = aSig;
    else
      sigma[iDev] = sigma[iDev - 1] + aSig;
      
    beta[iDev] ~ uniform(beta_lower, beta_upper);
  }
  beta[numDevs] = 0;
  
  for (iAY in 1:numAY){
    alpha[iAY] ~ normal(log_premium[iAY] + log_elr, alpha_sd);
  }
  
  for (iObs in 1:numObs){
    mu[iObs] = alpha[AY_Index[iObs]] + beta[Lag[iObs]] * (1 - gamma) ^ (AY_Index[iObs] - 1);
    
    log_loss ~ normal(mu, sig);
  }
}