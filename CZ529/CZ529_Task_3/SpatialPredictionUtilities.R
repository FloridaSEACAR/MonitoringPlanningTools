
# CZ529  Deliverable 3a: Evaluation of sample size to assess managed-area-level trends
# Utilities for spatial prediction
# this prediction function is based on the spatial_mean_RE.stan model
# functions to assess standard deviation as a function of nuber of GRTS samples.

library(raster) # for distance calculations
library(spsurvey) # for grts
predict_mean <- function(site_mean, alpha, rho, 
                       obs_distances, pred_distances,
                       obs_to_pred_distances){
  epsilon = 1e-6 #1e-4
  # Estimated covariance kernel for the fitted points
  K_obs <- alpha ^ 2 * exp(-0.5 * ((obs_distances / rho) ^ 2)) +
    diag(epsilon, dim(obs_distances)[1])
  
  # Cross-covariance between prediction points and fitted points
  # non diagonal matrix so no diagonal added. Estimate is very sensitive
  # to that diagonal. For example, need to include it when
  # predicted positions == observation positions.
  K_new <- alpha ^ 2 * exp(-0.5 * ((obs_to_pred_distances / rho) ^ 2)) 
   
  
  # Estimated covariance kernel for prediction points
  K_star <- alpha ^ 2 * exp(-0.5 * ((pred_distances / rho) ^ 2)) +
    diag(epsilon, dim(pred_distances)[1])
  
  # Estimated mean for prediction points
  t(K_new) %*% solve(K_obs, site_mean) # site_mean = global_global_mean + global_mean + gp_function
}



my_calc_point_distances = function(coordinates, coordinates2){
  if(missing(coordinates2)){
    distances <- raster::pointDistance(coordinates, lonlat = FALSE)
    distances[upper.tri(distances)] = t(distances)[upper.tri(distances)]
  } else {
    distances <- raster::pointDistance(coordinates, coordinates2, lonlat = FALSE, allpairs = TRUE)
  }
  return(distances)
}

# legacy_data was used to do the stan fit
SpatialPrediction = function(fit, legacy_positions, new_positions){

alpha_posterior = fit$draws("gp_sigma", format = "draws_matrix") # 4 chains 5000 draws after 5000 warmup
rho_posterior = fit$draws("length_scale", format = "draws_matrix")
obs_gp_function_posterior = fit$draws("gp_function", format = "draws_matrix")
obs_distances =  my_calc_point_distances(legacy_positions)
pred_distances =  my_calc_point_distances(new_positions)
obs_to_pred_distances = my_calc_point_distances(legacy_positions, new_positions)
site_mean_posterior = fit$draws("site_mean", format = "draws_matrix")

# 100 draws
# each row is a sample from the posterior distributions
# obs_distances is the pairwise distances of the observations
# pred_distances is the pairwise distances of the new positions to predict at

mean_preds <- matrix(NA, nrow = 100, ncol = NROW(new_positions))
for(i in 1:NROW(mean_preds)){
  #cat('Processing posterior draw', i, 'of 5...\n')
  mean_preds[i,] <- predict_mean(as.vector(as.matrix(site_mean_posterior[i,])), 
                                 alpha = alpha_posterior[i],
                                 rho = rho_posterior[i],
                                obs_distances = obs_distances,
                             pred_distances = pred_distances,
                             obs_to_pred_distances = obs_to_pred_distances
                             )
}  
  
return(mean_preds)

}


SpatialPredictionMeanOnly = function(fit, legacy_positions, new_positions){

site_mean_mean = fit$summary("site_mean")$mean
alpha_mean = fit$summary("gp_sigma")$mean 
rho_mean = fit$summary("length_scale")$mean
obs_gp_function_mean = fit$summary("gp_function")$mean
obs_distances =  my_calc_point_distances(legacy_positions)
pred_distances =  my_calc_point_distances(new_positions)
obs_to_pred_distances = my_calc_point_distances(legacy_positions, new_positions)

  mean_preds <- predict_mean(site_mean_mean
                             , alpha = alpha_mean
                             , rho = rho_mean
                             , obs_distances = obs_distances
                             , pred_distances = pred_distances
                             , obs_to_pred_distances = obs_to_pred_distances
                             )
  
return(mean_preds)

}




SpatialPredictionCovariate = function(fit, legacy_positions, new_positions, new_covariate){
  
alpha_covariate_posterior = fit$draws("gp_sigma", format = "draws_matrix") # 4 chains 5000 draws after 5000 warmup
rho_covariate_posterior = fit$draws("length_scale", format = "draws_matrix")
obs_distances =  my_calc_point_distances(legacy_positions)
pred_distances =  my_calc_point_distances(new_positions)
obs_to_pred_distances = my_calc_point_distances(legacy_positions, new_positions)
global_mean_posterior = fit$draws("global_mean", format = "draws_matrix")
global_global_mean_posterior = fit$draws("global_global_mean", format = "draws_matrix")
site_mean_posterior = fit$draws("site_mean", format = "draws_matrix")
slope_posterior1 = fit$draws("slope[1]", format = "draws_matrix")
slope_posterior2 = fit$draws("slope[2]", format = "draws_matrix")

# 100 draws
# each row is a sample from the posterior distributions
# obs_distances is the pairwise distances of the observations
# pred_distances is the pairwise distances of the new positions to predict at

mean_covariate_preds <- matrix(NA, nrow = 100, ncol = NROW(new_positions))
for(i in 1:NROW(mean_covariate_preds)){
  #cat('Processing posterior draw', i, 'of 5...\n')
  # legacy covariate already in site_mean because of the model fit
  mean_covariate_preds[i,] <- predict_mean(as.numeric(site_mean_posterior[i, ])
                              , alpha = alpha_covariate_posterior[i]
                              , rho = rho_covariate_posterior[i]
                              , obs_distances = obs_distances
                              , pred_distances = pred_distances
                              , obs_to_pred_distances = obs_to_pred_distances) 
    mean_covariate_preds[i,] = mean_covariate_preds[i,] + slope_posterior1[i] *new_covariate + slope_posterior2[i]*new_covariate*new_covariate
}
                              
  
return(mean_covariate_preds)

}




SD_by_number_of_reefs = function(maop = managed_area_oyster_points, omapmp = oyster.managed_area_parameter.mean_preds, legacy_positions = legacy_positions, fit = fit.spatial_mean_RE.Managed_Area_Parameter){
  require(spsurvey) 
  if( 2+nrow(legacy_positions) > nrow(maop)) {print("need at least 2 positions to draw"); return(NULL)}
  max_interations = min(nrow(maop),nrow(legacy_positions)+50) - nrow(legacy_positions)
pb = txtProgressBar(min = 2, max = max_interations, initial = 2, style = 3)
GRTS_nonuniform_sampling_results = data.frame() 
GRTS_nonuniform_sampling_results_mean_only = data.frame()

 tmp_manage_area_parameter_oyster.positions.sd.sf =st_as_sf(data.frame(maop, sd = apply(omapmp,2,sd), var  = apply(omapmp,2,var)), coords = c("X","Y"), crs =  st_crs("WGS84"))

 for( sample_size in 2:max_interations)
  { 
  for( iterations in 1:50){
    tmp_new_grts_positions = as.data.frame(st_coordinates(grts(tmp_manage_area_parameter_oyster.positions.sd.sf,n_base = sample_size  +  nrow(legacy_positions), projcrs_check = FALSE, aux_var = "var", legacy_sites = st_as_sf(legacy_positions, coords = c(1:2), crs =  st_crs("WGS84")) %>% mutate(var = 1))$sites_base)
  )
    tmp_mean_preds = SpatialPrediction(fit = fit, legacy_positions, tmp_new_grts_positions)
  # take the row means - this represents an estimate of the global mean from the additional GRTS samples
  # the sd represents the estimation error -- these are draws from the posterior
    GRTS_nonuniform_sampling_results = rbind(GRTS_nonuniform_sampling_results, data.frame(sample_size = sample_size, sd = sd(apply(tmp_mean_preds,1,mean))))
  }
   setTxtProgressBar(pb,sample_size)
 }

close(pb)
  names(GRTS_nonuniform_sampling_results) = c("number_of_sites","sd")

  return(GRTS_nonuniform_sampling_results)
}


# this version is used in the display; it does not use the posteriors of the fits, which adds a tremendous amount of noise.
SD_by_number_of_reefs_no_posteriors = function(maop = managed_area_oyster_points, omapmp = oyster.managed_area_parameter.mean_preds, legacy_positions = legacy_positions, fit = fit.spatial_mean_RE.Managed_Area_Parameter){
 
  if( 2+nrow(legacy_positions) > nrow(maop)) {print("need at least 2 positions to draw"); return(NULL)}
  max_interations = min(nrow(maop),nrow(legacy_positions)+50) - nrow(legacy_positions)
pb = txtProgressBar(min = 2, max = max_interations, initial = 2, style = 3)
GRTS_nonuniform_sampling_results = data.frame() 
GRTS_nonuniform_sampling_results_mean_only = data.frame()

 tmp_manage_area_parameter_oyster.positions.sd.sf =st_as_sf(data.frame(maop, sd = apply(omapmp,2,sd), var  = apply(omapmp,2,var)), coords = c("X","Y"), crs =  st_crs("WGS84"))
 
 alpha_mean = fit$summary("gp_sigma")$mean
 rho_mean = fit$summary("length_scale")$mean
 obs_distances =  my_calc_point_distances(legacy_positions)

 site_mean_mean = fit$summary("site_mean")$mean

 # sample size needs to be include the number of legacy sites
 for( sample_size in 2:max_interations)
  { 
  for( iterations in 1:50){
    
     tmp_new_grts_positions = as.data.frame(st_coordinates(grts(tmp_manage_area_parameter_oyster.positions.sd.sf,n_base = sample_size + nrow(legacy_positions), projcrs_check = FALSE, aux_var = "var", legacy_sites = st_as_sf(legacy_positions, coords = c(1:2), crs =  st_crs("WGS84")) %>% mutate(var = 1))$sites_base)
  )
    
    pred_distances =  my_calc_point_distances(tmp_new_grts_positions)
    obs_to_pred_distances = my_calc_point_distances(legacy_positions, tmp_new_grts_positions)
 
    tmp_mean_preds = predict_mean(site_mean = site_mean_mean
                                     , alpha_mean
                                     , rho_mean
                                     , obs_distances
                                     , pred_distances
                                     , obs_to_pred_distances
                                      ) # simply the mean estimate, no posteriors; mean estimate at the grts sites
     # std deviation of the grts estimates; mean(tmp_mean_preds) is an estimate of the global mean
  GRTS_nonuniform_sampling_results = rbind(GRTS_nonuniform_sampling_results, data.frame(sample_size = sample_size, m = mean(tmp_mean_preds)))
  
  }
   setTxtProgressBar(pb,sample_size)
 }
close(pb)
  names(GRTS_nonuniform_sampling_results) = c("number_of_sites","m")

  return(GRTS_nonuniform_sampling_results %>% group_by(number_of_sites) %>% dplyr::summarise(sd = sd(m)))

}

