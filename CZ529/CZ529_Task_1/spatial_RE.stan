// CZ529 Task 3: Evaluation of sample size to assess managed-area-level trends Deliverable 3a:
//
// geostatistical model 
// where site means have a covariate
// 

data {
  int<lower=1> N;
  int <lower=1> G; // number of sites
  vector[N] y; // response
  vector[G] x; // covariate per site
  array[N] int group;  // map data to group
  array[G] vector[2] positions;//  positions in lon lat

}

transformed data {
  // Small offset to ensure the covariance matrix is positive definite
  real delta = 1e-9;
}

parameters {
   
  
  real global_global_mean; //overall managed area mean <- this could represent mean oyster shell height for example.
  
  // these are for the gaussian process  
  real<lower=0> length_scale; // a.k.a rho
  real<lower=0> gp_sigma; // a.k.a alpha
  vector[G] eta;
  
  real<lower=0> sigma;
  real slope; // for covariate
  

  
}

transformed parameters {
  // Calculate the latent Gaussian process function (phi)

  matrix[G, G] K = gp_exp_quad_cov(positions, gp_sigma, length_scale); 
  matrix[G, G] L_K = cholesky_decompose(K + diag_matrix(rep_vector(delta, G))); 
  vector[G] gp_function = L_K * eta; 
  vector[G] global_mean = slope * x; 
  vector[G] site_mean = global_global_mean + global_mean + gp_function;
}



model {
  // need a strong prior on length_scale which 
  // governs the spatial decay
  // small values make the prediction unstable
  length_scale ~ normal(2, 0.2);  
  // Prior for the GP covariance magnitude
  gp_sigma ~ std_normal();
  
  eta ~ std_normal();
  sigma ~ cauchy(0,1);
  slope ~ normal(0,10);
  global_mean ~ normal(0,10);
  global_global_mean ~ normal(0,10); 
  vector[N] mu; 
 
for (n in 1:N)
  mu[n] = site_mean[group[n]];

y ~ normal(mu, sigma);

}



