library(tidyverse)
library(splithalf)
source("helper_functions.R")

analysis_data <- data.table::fread("data/analysis_data.csv")

prep <- analysis_data %>% 
  filter(
    repetition_time < 20,
    n_statements > 50,
    n_statements < 70
  ) %>% 
  group_by(procedure_id, truth_rating_scale, n_statements) %>% 
  nest()

# data <- prep$data[[1]]

data <- prep %>% 
  mutate(
    truth_effect = map(data, compute_subject_truth_effect)
  ) %>% 
  mutate(
    stats = map(truth_effect, get_distribution_stats),
  )

data <- data %>% 
  mutate(
    reliability = map(data, compute_reliability)
  )

data <- data %>% 
  mutate(
    sb_estimate = map_dbl(reliability, ~ .x$final$spearmanbrown),
    sb_low = map_dbl(reliability, ~ .x$final$SB_low),
    sb_high = map_dbl(reliability, ~ .x$final$SB_high),
    var = map_dbl(stats, ~.x$var)
  )

## - Plot distributions
analysis_data %>% 
  group_by(procedure_id, truth_rating_scale, subject, repeated) %>% 
  summarize(
    mean_truth = mean(response, na.rm = TRUE)
  ) %>% 
  pivot_wider(
    id_cols = c("subject", "procedure_id", "truth_rating_scale"),
    values_from = "mean_truth",
    names_from = repeated,
    names_prefix = "repeated_"
  ) %>% 
  mutate(
    truth_effect = repeated_1 - repeated_0
  ) %>% 
  ggplot(
    aes(
      x = truth_effect,
      fill = truth_rating_scale
    )
  )+
  geom_density()+
  facet_wrap(~procedure_id)

## Plot variance
ggplot(data, aes(x = fct_reorder(factor(procedure_id), sb_estimate), y = var, color = truth_rating_scale)) +
  geom_point(mapping = aes(size = n_statements)) +
  # geom_errorbar(aes(ymin = sb_low, ymax = sb_high), width = 0.2) +
  # coord_flip() +  # makes it easier to read if many levels
  labs(
    x = "Experiment ID",
    y = "Variance",
    title = "Variance by Truth Rating Scale",
    subtitle = "Experiment IDs sorted by Reliability"
  ) +
  theme_minimal()  

## Plot sb estimates
data %>% 
  ggplot(
    aes(x = sb_estimate, fill = truth_rating_scale)
  )+
  geom_density(
    alpha = 0.5
  )+
  labs(
    x = "Spearman-Brown Estimate",
    title = "Split-Half Reliability by Truth Rating Scale"
  )

ggplot(data, aes(x = fct_reorder(factor(procedure_id), sb_estimate), y = sb_estimate, color = truth_rating_scale)) +
  geom_point(mapping = aes(size = n_statements)) +
  geom_errorbar(aes(ymin = sb_low, ymax = sb_high), width = 0.2) +
  # coord_flip() +  # makes it easier to read if many levels
  labs(
    x = "Experiment ID",
    y = "Spearman-Brown Estimate",
    title = "Split-Half Reliability by Truth Rating Scale"
  ) +
  theme_minimal()  

## Plot sb estimates by trial
data %>% 
  # filter(n_statements < 170) %>% 
  ggplot(aes(x = n_statements, y = sb_estimate, color = truth_rating_scale,
                 group = truth_rating_scale)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = sb_low, ymax = sb_high), width = 0.2) +
  # geom_smooth(method = "lm", se = FALSE)+
  # coord_flip() +  # makes it easier to read if many levels
  labs(
    x = "# of statements",
    y = "Spearman-Brown Estimate",
    title = "Split-Half Reliability by # of Statements"
  ) +
  theme_minimal() 




