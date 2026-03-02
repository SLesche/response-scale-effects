dich_model <- function(data){
  model <- lme4::glmer(
    Y ~ cond + (1 + cond || sub),
    data = data,
    family = 'binomial'
  )
  
  return(model)
}

estimate_var <- function(fit){
  
}

icc_within_observed <- function(fit, data, 
                                nsim = 2000,
                                subject_re = "sub",
                                random_effects = c("Intercept")) {
  # Extract posterior draws
  draws <- as.data.frame(fit)
  
  # Extract fixed effects
  fe_names <- names(fixef(fit)[,1])
  betas <- draws[, paste0("b_", fe_names), drop = FALSE]
  
  # Extract SDs of subject-level random effects
  sd_names <- paste0("sd_", subject_re, "__", random_effects)
  sd_sub <- draws[, sd_names, drop = FALSE]
  
  n_draws <- nrow(draws)
  n_subjects <- length(unique(data[[subject_re]]))
  n_obs <- nrow(data)
  
  icc_draw <- numeric(n_draws)
  var_between_draw <- numeric(n_draws)
  var_within_draw <- numeric(n_draws)
  
  for(s in 1:n_draws){
    # 1. Simulate random effects
    # Handle multiple REs (Intercept + slope)
    if(ncol(sd_sub) == 1){
      u_i <- rnorm(n_subjects, mean = 0, sd = sd_sub[s,1])
      re_mat <- u_i[data[[subject_re]]]
    } else {
      # Correlated REs (Intercept + slopes)
      # get covariance matrix
      cor_name <- paste0("cor_", subject_re, "__", random_effects[1], "__", random_effects[2])
      rho <- draws[s, cor_name]
      sd_int <- sd_sub[s,1]
      sd_slope <- sd_sub[s,2]
      Sigma <- matrix(c(sd_int^2, rho*sd_int*sd_slope,
                        rho*sd_int*sd_slope, sd_slope^2), nrow=2)
      u_mat <- MASS::mvrnorm(n_subjects, mu = c(0,0), Sigma = Sigma)
      re_mat <- u_mat[data[[subject_re]], ]
    }
    
    # 2. Compute linear predictor for each observation
    eta <- as.numeric(as.matrix(data[,fe_names]) %*% as.numeric(betas[s,]))
    if(ncol(sd_sub) == 1){
      eta <- eta + re_mat
    } else {
      eta <- eta + rowSums(re_mat * as.matrix(data[, random_effects]))
    }
    
    # 3. Predicted probabilities
    p <- plogis(eta)
    
    # 4. Within-participant conditional variance
    var_within_draw[s] <- mean(p * (1 - p))
    
    # 5. Between-participant variance in predicted probabilities due to random effects
    subj_means <- tapply(p, data[[subject_re]], mean)
    var_between_draw[s] <- var(subj_means)
    
    # 6. ICC-like ratio
    icc_draw[s] <- var_between_draw[s] / (var_between_draw[s] + var_within_draw[s])
  }
  
  return(list(
    icc = icc_draw,
    var_between = var_between_draw,
    var_within = var_within_draw,
    icc_median = median(icc_draw),
    var_between_median = median(var_between_draw),
    var_within_median = median(var_within_draw)
  ))
}