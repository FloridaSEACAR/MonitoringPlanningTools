// CZ529 Task 3a.

// random effects linear model 
// where means have a geostatistical model
 // https://mc-stan.org/docs/stan-users-guide/gaussian-processes.html

data {
  int<lower=1> N;
  vector[N] y; // shell heights
  int<lower=1> G; // number of groups/sites
  array[N] int group;// map data to group
  array[G] vector[2] positions; 
}

transformed data {
  // Small offset to ensure the covariance matrix is positive definite
  real delta = 1e-9;
  
}

parameters {
  vector[G] global_mean;
  real global_global_mean; //overall managed area mean <- this could represent mean oyster shell height for example.
  
  real<lower=0> sigma;

  // these are for the gaussian process  
  real<lower=0> length_scale; // a.k.a rho
  real<lower=0> gp_sigma; // a.k.a alpha
  vector[G] eta;
  
}

transformed parameters {
  // Calculate the latent Gaussian process function (phi)
  matrix[G,G] L_chol = gp_exp_quad_cov( positions,gp_sigma,length_scale) + diag_matrix(rep_vector(delta, G));
    vector[G] gp_function = cholesky_decompose(L_chol) * eta;
    vector[G] site_mean = global_global_mean + global_mean + gp_function; 
}



model {
  
  length_scale ~ normal(0, 2.5); 
  // Prior for the GP covariance magnitude
  gp_sigma ~ std_normal();
  // Multiplier for non-centred GP parameterisation
  eta ~ std_normal();
  sigma~ cauchy(0,1);

  global_mean ~ normal(0,10);
  global_global_mean ~ normal(0,10); // new
  
  vector[N] mu;
 
  for (n in 1:N)
    mu[n] = site_mean[group[n]];

   y ~ normal(mu, sigma);

}






