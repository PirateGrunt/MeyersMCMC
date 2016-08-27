#========================================================================
# Script to run the CSR Model on data from the CAS Loss Reserve Database
# Original script by Glenn Meyers
# Modified by Brian A. Fannin
#========================================================================

source("Common.R")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dfSchedP <- GetAllTriangles("comauto") %>% 
  raw::CasColNames(restore = FALSE)

dfTriangle <- SingleTriangle(dfSchedP, "353")

dfUpper <- dfTriangle %>% 
  filter(Upper, Lag <=9)

premium <- dfTriangle %>% 
  filter(Lag == 1) %>% 
  select(NetEP) %>% 
  unlist()

fit <- lm(CumulativeIncurred ~ 0 + NetEP, data = dfUpper)

stanData <- list(numObs = nrow(dfUpper)
                 , numAY = length(unique(dfUpper$AY_Index))
                 , numLags = length(unique(dfUpper$Lag))
                 , premium = premium
                 , loss = dfUpper$CumulativeIncurred
                 , AY_Index = dfUpper$AY_Index
                 , lag = dfUpper$Lag
                 , elr_lower = 0.05
                 , elr_upper = 4
                 , sd_alpha = sqrt(10))

fitLCL <- stan(file = 'LCL.stan', data = stanData, seed = 1234)

elr <- extract(fitLCL, 'elr') %>% unlist()
summary(elr)

pltELR <- ggplot(as.data.frame(elr), aes(elr)) + geom_density(fill = "grey")
pltELR <- pltELR + geom_vline(color = "red", xintercept = 0.6) + xlim(0, 5)
pltELR

coef(fit)
mean(elr)
