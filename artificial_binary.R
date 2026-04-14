library(tidyverse)
library(splithalf)
source("helper_functions.R")

analysis_data <- data.table::fread("data/analysis_data.csv")

prep <- analysis_data %>% 
  # filter(
  #   repetition_time < 20,
  #   n_statements > 50,
  #   n_statements < 70
  # ) %>% 
  group_by(procedure_id, truth_rating_scale, n_statements) %>% 
  nest()

data <- prep %>% 
  mutate(
    has_certainty = map_dbl(
      data, ~ifelse(
        all(is.na(.x$certainty)),
        0,
        1
      )
    )
  ) %>% 
  mutate(
    has_art = ifelse(
      truth_rating_scale != "dichotomous" || has_certainty,
      1,
      0
    )
  ) %>% 
  mutate(
    art_data = map2(data, truth_rating_scale, transform_artificial_responses)
  ) %>% 
  mutate(
    truth_effect = map(data, compute_subject_truth_effect),
    art_truth_effect = map(art_data, compute_subject_truth_effect),
  ) %>% 
  mutate(
    stats = map(truth_effect, get_distribution_stats),
    art_stats = map(art_truth_effect, get_distribution_stats),
  )

data <- data %>% 
  mutate(
    effsize = map(data, compute_effsize),
    art_effsize = map(art_data, compute_effsize),
    certainty_effsize = map(data, compute_certainty_effsize)
  )

data <- data %>% 
  mutate(
    d_estimate = map_dbl(effsize, ~ .x$Cohens_d),
    d_low = map_dbl(effsize, ~ .x$CI_low),
    d_high = map_dbl(effsize, ~ .x$CI_high),
    art_d_estimate = map_dbl(art_effsize, ~ ifelse(has_art, .x$Cohens_d, NA)),
    art_d_low = map_dbl(art_effsize, ~ ifelse(has_art, .x$Cohens_d, NA)),
    art_d_high = map_dbl(art_effsize, ~ ifelse(has_art, .x$Cohens_d, NA)),
    cert_d_estimate = map_dbl(certainty_effsize, ~ ifelse(has_certainty, .x$Cohens_d, NA)),
    cert_d_low = map_dbl(certainty_effsize, ~ ifelse(has_certainty, .x$CI_low, NA)),
    cert_d_high = map_dbl(certainty_effsize, ~ ifelse(has_certainty, .x$CI_high, NA))
  )

data <- data %>% 
  mutate(
    reliability = map(data, compute_reliability),
    art_reliability = map(art_data, compute_reliability)
  )

data <- data %>% 
  mutate(
    sb_estimate = map_dbl(reliability, ~ .x$final$spearmanbrown),
    sb_low = map_dbl(reliability, ~ .x$final$SB_low),
    sb_high = map_dbl(reliability, ~ .x$final$SB_high),
    var = map_dbl(stats, ~.x$var),
    art_sb_estimate = map_dbl(art_reliability, ~ ifelse(has_art, .x$final$spearmanbrown, NA)),
    art_sb_low = map_dbl(art_reliability,  ~ ifelse(has_art, .x$final$SB_low, NA)),
    art_sb_high = map_dbl(art_reliability,  ~ ifelse(has_art, .x$final$SB_high, NA)),
    art_var = map_dbl(art_stats, ~.x$var)
  )

clean_data <- data %>% 
  select(-data, -art_data)

saveRDS(clean_data, file = "data/reliability_data.Rdata")
