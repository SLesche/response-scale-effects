# Functions for Computing the Gamma Coefficient -------------------------------
# Taken from Zajdler & Schnuerch (2025)
# - https://osf.io/me367/files/4dnsm?view_only=95f215026d0d4085a0348854df91f44f

# load packages
library(brms)
library(lme4)


## Approach 1: Method of Moments (Rouder & Mehrvarz, 2024) --------------------

mom <- function(df) {
  I <- length(unique(df$sub))
  L <- round(mean(unique(table(df$sub, df$cond)))) # take mean and round because some studies have a slightly different number of trials per conditions
  
  df <- df |> 
    group_by(sub, cond) |> 
    mutate(Ybar = mean(Y),
           dev = Y - Ybar)
  
  mse <- sum(df$dev^2) / (I * 2 * L - I * 2)
  
  vard <- df |> summarize(mean = mean(Y), .groups = "drop") |> 
    pivot_wider(
      names_from = cond,
      values_from = mean
    ) |> 
    mutate(d = `0.5` - `-0.5`) |> 
    pull() |> 
    var()
  
  # gamma^2
  gamma2 <- vard / mse - 2/L
  
  # gamma
  gamma <- sqrt(vard / mse - 2/L)
  
  # return variance components and gamma
  return(list("mse" = mse, 
              "vard" = vard,
              "L" = L,
              "gamma_square" = gamma2, 
              "gamma" = gamma))
}



## Approach 2: Hierarchical Modeling ------------------------------------------

### Option 1: Bayesian --------------------------------------------------------

sample_variances <- function(df) {
  fit <- brm(
    Y ~ cond + (1 + cond || sub), 
    data = df,
    chains = 8,
    iter = 3000,
    warmup = 1000,
    cores = 8
  )
  
  df_fit <- as.data.frame(fit)
  
  return(data.frame("sigma" = df_fit$sigma, 
                    "sd_sub__cond" = df_fit$sd_sub__cond))
}

sample_variances_scale <- function(df) {
  fit <- brm(
    Y ~ cond + (1 + cond || sub) + (1 | item), 
    data = df,
    chains = 4,
    iter = 3000,
    warmup = 1000,
    cores = 4
  )
  
  df_fit <- as.data.frame(fit)
  
  return(data.frame("sigma" = df_fit$sigma, 
                    "sd_sub__cond" = df_fit$sd_sub__cond))
}

sample_variances_binary <- function(df) {
  fit <- brm(
    Y ~ cond + (1 + cond || sub) + (1 | item), 
    data = df,
    chains = 4,
    iter = 3000,
    warmup = 1000,
    cores = 4
  )
  
  sigma_latent <- sqrt(pi^2 / 3)  # acts like sigma in Gaussian
  
  df_fit <- as.data.frame(fit)
  
  return(data.frame("sigma" = sigma_latent, 
                    "sd_sub__cond" = df_fit$sd_sub__cond))
}

fit_binary_model <- function(df) {
  library(brms)
  
  fit <- brm(
    Y ~ cond + (1 + cond || sub) + (1 | item), 
    data = df,
    chains = 4,
    iter = 3000,
    warmup = 1000,
    cores = 4
  )
  
  return(fit)
}

compute_gamma <- function(df_fit) {
  
  # variance estimates
  sigma_w <- mean(df_fit$sigma)
  sigma_b <- mean(df_fit$sd_sub__cond)
  
  # gamma^2
  gamma2 <- (sigma_b / sigma_w)^2
  
  # gamma
  gamma <- sigma_b / sigma_w
  
  # posterior distribution of gamma
  gamma_distr <- df_fit$sd_sub__cond / df_fit$sigma
  
  # return variance and gamma estimates and gamma distribution
  return(list("sigma_w" = sigma_w,
              "sigma_b" = sigma_b,
              "gamma" = gamma,
              "gamma_square" = gamma2,
              "gamma_distribution" = gamma_distr
  ))
}


### Option 2: Frequentist -----------------------------------------------------

estimate_gamma_freq <- function(df) {
  fit <- lmer(
    Y ~ cond + (1 + cond || sub), 
    data = df
  )
  tmp <- summary(fit)
  
  sigma_w <- tmp$sigma
  sigma_b <- sqrt(tmp$varcor$sub.1[1])
  
  # gamma^2
  gamma2 <- (sigma_b / sigma_w)^2
  
  # gamma
  gamma <- sigma_b / sigma_w
  
  return(list("sigma_w" = sigma_w,
              "sigma_b" = sigma_b,
              "gamma_square" = gamma2, 
              "gamma" = gamma))
}


## Considering Item Effects ---------------------------------------------------
# only works for data sets where item information is available

gamma_with_items_n <- function(df) {
  fit <- brm(
    Y ~ cond + statement_accuracy + (1 + cond + statement_accuracy || sub) + (1 || item), # why is statement_accuracy subject-specific?
    data = df,
    chains = 8,
    iter = 3000,
    warmup = 1000,
    cores = 8,
    control = list(adapt_delta = 0.9)
  )
  df_fit <- as.data.frame(fit)
  
  return(data.frame("sigma" = df_fit$sigma, 
                    "sd_sub__cond" = df_fit$sd_sub__cond))
}



# Compute Reliability dependent on gamma --------------------------------------

rel <- function(df) {
  # extract gamma and L
  gamma <- df$gamma
  L <- df$L
  
  # compute reliability (Rouder & Mehrvarz, 2024, equation (1))
  rel <- (gamma^2) / (gamma^2 + 2/L)
  
  return(rel)
}