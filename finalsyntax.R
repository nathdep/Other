needed_packages = c("invgamma", "ggplot2", "cmdstanr", "HDInterval", "bayesplot", “truncnorm”, “tidyverse”)
for(i in 1:length(needed_packages)){
  haspackage = require(needed_packages[i], character.only = TRUE)
  if(haspackage == FALSE){
    install.packages(needed_packages[i])
  }
  library(needed_packages[i], character.only = TRUE)
}


   
set.seed(777)

mu = rnorm(n=20,mean=0,sd=1) # Generating 100 random intercepts from normal(0,1)
lambda = rtruncnorm(n=20, a=0, b= 3, mean=0, sd=1) # Generating 100 random slopes from normal(0,1)
theta1 = rnorm(n=1000, mean=0, sd=1) # Generating 1000 random thetas for Group 1 from normal(0,1)
theta2 = rnorm(n=1000, mean=-1, sd = .75) # Generating 1000 random thetas for Group 2 from normal(-1,.75) to simulate group differences
theta = c(theta1, theta1) # Merging thetas from both groups into one vector
isGroup = c(rep(0, times=1000), rep(1, times = 1000)) # Dummy coding variable for group membership

# loop over each item to generate data
nItems = length(mu)
nObs = length(theta)
dataMat = matrix(data = NA, nrow = nObs, ncol = nItems)


for (obs in 1:nObs){
  for (item in 1:nItems){
    logit = mu[item] + lambda[item]*theta[obs]
    prob = exp(logit)/(1+exp(logit))
    dataMat[obs, item] = rbinom(n = 1, size = 1, prob = prob)
  }
  
}

dataMatcheck = dataMat %>% 
  as.data.frame()

dataMatcheck$isGroup <- isGroup 

colnames(dataMatcheck) <- c(paste0("Q", 1:20), "isGroup")

# data check (creates summary outputs for logistic regression models for each question)
 modlist = lapply(paste0("Q",1:20,"~ theta"), as.formula)
 modelresults = lapply(modlist, function(x) glm(x, family = binomial(link = "logit"), data = dataMatcheck))
modelsummaries = lapply(modelresults, summary)

# begin rstan syntax

synt = '

data{
  int<lower=1> nItems; //number of items in test
  int<lower=1> nObs; // number of students
  vector[nObs] isGroup; // dummy-coded group membership vector
  array[nItems, nObs] int<lower=0, upper=1> Y; // outcome (response)
    vector[nItems] meanMu;             // prior mean vector for intercept parameters
  matrix[nItems, nItems] covMu;      // prior covariance matrix for intercept parameters
  vector[nItems] meanLambda;         // prior mean vector for discrimination parameters
  matrix[nItems, nItems] covLambda;  // prior covariance matrix for discrimination parameters


  
  
}

parameters{
  vector [nObs] theta; // latent ability parameter
  vector [nItems] lambda; // slope parameter
  vector [nItems] mu; // intercept parameter
  vector [nItems] betaisGroup; // group difference in intercept parameter
  real groupmeantheta; // parameter for Group 2's latent ability mean value
  real grouplogsdtheta; // parameter for Group 2's log-transformed standard deviation value



}


model{
  theta ~ normal(0+groupmeantheta*isGroup,exp(0+grouplogsdtheta*isGroup));
  lambda ~ multi_normal(meanLambda, covLambda);
  mu ~ multi_normal(meanMu, covMu);
  grouplogsdtheta ~ normal(0,1);
  groupmeantheta ~ normal(0,1);
  betaisGroup ~ normal(0,1);
 
 

  
  for (item in 1:nItems){
    Y[item] ~ bernoulli_logit(mu[item] + lambda[item]*theta + betaisGroup[item]*isGroup);
  }
  

}


generated quantities{
  vector[nItems] a; // IRT discrimination parameter
  vector[nItems] b; // IRT difficulty parameter

  
  for(item in 1:nItems){
    a[item] = lambda[item];
    b[item] = -1*mu[item]/lambda[item];
  }
  
}
'
 

   
stanmod = cmdstan_model(stan_file = write_stan_file(synt)) // compile rstan syntax
 

   
nItems = ncol(dataMat)
nObs = nrow(dataMat)
isGroup = isGroup
Y = t(dataMat)

muMeanHyperParameter = 0
muMeanVecHP = rep(muMeanHyperParameter, nItems)

muVarianceHyperParameter = 1
muCovarianceMatrixHP = diag(x = muVarianceHyperParameter, nrow = nItems)

# item discrimination/factor loading hyperparameters
lambdaMeanHyperParameter = 0
lambdaMeanVecHP = rep(lambdaMeanHyperParameter, nItems)

lambdaVarianceHyperParameter = 1
lambdaCovarianceMatrixHP = diag(x = lambdaVarianceHyperParameter, nrow = nItems)


# Create rstan model data

modeldata <- list(
  nObs = nObs,
  nItems = nItems,
  isGroup = isGroup,
  Y = Y,
  meanLambda = lambdaMeanVecHP,
  covLambda = lambdaCovarianceMatrixHP,
  meanMu = muMeanVecHP,
  covMu = muCovarianceMatrixHP
)


# Run MCMC estimation
  
modelrun <- stanmod$sample(
  data = modeldata,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  seed = 777,
  init = function() list(lambda = rnorm(nItems, 10, 1)) # inital values to prevent bimodal posterior
  )

sum = summary(modelrun$draws()) # save summary of sampled parameter values to object
 