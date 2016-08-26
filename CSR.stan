data {
  // sample data
  int<lower=1> numAY;
  int<lower=1> numDevs;
  int<lower=1> numObs;
  vector[numAY] premium;

  vector[numObs] cumulative_loss;
  int AY_Index[numObs];
  int Lag[numObs];
  
  // New predicted quantities

}

transformed data {
  vector[numObs] log_loss;
  vector[numAY] log_premium;
  
  log_loss = log(cumulative_loss);
  log_premium = log(premium);
}

parameters {
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
  
  // real elr;
  
}

transformed parameters {
  
  // log_elr = log(elr);
}

model {
  int iAY;
  int iLag;
  
  real log_elr;
  real aSig;
  real sigma[numDevs];
  
  real gamma;               // gamma is a parameter, not a function here
  
  vector[numAY] alpha;
  vector[numDevs] beta;
  vector[numObs] mu;
  
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
  
  for (nAY in 1:numAY){
    alpha[nAY] ~ normal(log_premium[nAY] + log_elr, alpha_sd);
  }
  
  for (iObs in 1:numObs){
    iAY = AY_Index[iObs];
    iLag = Lag[iObs];
    mu[iObs] = alpha[iAY] + beta[iLag] * (1 - gamma) ^ (AY_Index[iObs] - 1);
    
    log_loss ~ normal(mu[iObs], sigma[iLag]);
  }
}