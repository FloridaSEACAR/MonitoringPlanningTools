// CZ529 task 1a. 

// random effects linear model 
// where means do not have a geostatistical model
// this is used to compare to geostatistical model with a covariate
// and to geostatistical model without a covariate


data {
  int<lower=1> N;
  vector[N] y; // shell heights
  int<lower=1> G; // number of groups/sites
  vector[G] x; // covariate per site
  array[N] int group; // map data to group
}


parameters {
  real global_global_mean;
  real slope;
  real<lower=0> sigma;
  }


transformed parameters {
  vector[G] site_mean = global_global_mean + slope * x;
}


model {
  sigma~ cauchy(0,1);
  slope ~ normal(0,10);
  vector[N] mu; 
  for (n in 1:N)
    mu[n] = site_mean[group[n]];
    y ~ normal(mu, sigma);
}
