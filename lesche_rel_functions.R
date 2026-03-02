fit_binary_model <- function(data) {
  fit <- lme4::glmer(
    # Y ~ cond + (1 + cond || sub) + (1 | item), 
    Y ~ cond + (1 + cond || sub),
    data = data,
    family = "binomial"
  )
  
  return(fit)
}

compute_gamma <- function(model, data, nsim = 2000){
  # Draw out relevant model estimates
  ri_sd = as.data.frame(VarCorr(model))$sdcor[1]
  re_sd = as.data.frame(VarCorr(model))$sdcor[2]
  
  fix = fixef(model)
  
  subjects = unique(data$sub)
  n_subjects = length(subjects)
  
  var_between = numeric(nsim)
  var_within = numeric(nsim)
  
  cond_levels = unique(data$cond)
  
  for (i in 1:nsim){
    u_slope <- rnorm(n_subjects, 0, re_sd)
    u_intercept <- rnorm(n_subjects, 0, ri_sd)
    
    # Simulate RE slope only variance
    eta_slope_only <- matrix(0, nrow = n_subjects, ncol = length(cond_levels))
    for(j in seq_along(cond_levels)){
      eta_slope_only[,j] <- fix[1] + fix[2] * cond_levels[j] + u_slope * cond_levels[j]
    }
    p_slope_only <- plogis(eta_slope_only)
    
    # Subject-level mean probability (averaged across conditions)
    p_mean_slope_only <- rowMeans(p_slope_only)
    var_between[i] <- var(p_mean_slope_only)
    
    eta_full <- matrix(0, nrow = n_subjects, ncol = length(cond_levels))
    for(j in seq_along(cond_levels)){
      eta_full[,j] <- fix[1]+ u_intercept + fix[2] * cond_levels[j] + u_slope * cond_levels[j]
    }
    p_full <- plogis(eta_full)
    var_within[i] <- mean(p_full * (1 - p_full))
  }
  
  var_between_mean = mean(var_between)
  var_within_mean = mean(var_within)
  
  return(
    list(
      var_between_mean,
      var_within_mean,
      gamma = sqrt(var_between_mean) / sqrt(var_within_mean)
    )
  )
}
