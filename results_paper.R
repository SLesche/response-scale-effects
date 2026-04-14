library(tidyverse)
library(splithalf)
source("helper_functions.R")

data <- readRDS("data/reliability_data.Rdata")

## Plot dichotomized data ----
plot_data <- data %>% 
  filter(
    truth_rating_scale != "dichotomous"
  ) %>% 
  select(
    procedure_id,
    truth_rating_scale,
    n_statements,
    contains("sb_")
  )

order_levels <- plot_data %>%
  arrange(sb_estimate) %>%
  pull(procedure_id)

plot_data_long <- plot_data %>% 
  pivot_longer(
    cols = matches("^(art_)?(sb_|var)"),
    names_to = c("condition", ".value"),
    names_pattern = "^(art_)?(sb_.*|var)"
  ) %>% 
  mutate(
    condition = if_else(condition == "art_", "artificial", "control"),
    procedure_fac = factor(procedure_id, levels = order_levels)
    
  )

plot_data_long %>% 
  ggplot(
    aes(x = procedure_fac, y = sb_estimate, color = condition)) +
  # line connecting control ↔ artificial
  geom_line(aes(group = procedure_id), color = "grey70") +
  # points
  geom_point(size = 3) +
  # manual colors
  scale_color_manual(
    values = c(
      "control" = "blue",
      "artificial" = "red"
    )
  ) +
  coord_flip() +
  labs(
    x = "Procedure ID",
    y = "Spearman-Brown Estimate",
    color = "Condition",
    title = "Effect of Artificial Condition on Reliability"
  ) +
  theme_minimal()


## Plot likert data ----
plot_data <- data %>% 
  filter(
    truth_rating_scale == "dichotomous",
    has_certainty == 1
  ) %>% 
  select(
    procedure_id,
    truth_rating_scale,
    n_statements,
    contains("sb_")
  )

order_levels <- plot_data %>%
  arrange(sb_estimate) %>%
  pull(procedure_id)

plot_data_long <- plot_data %>% 
  pivot_longer(
    cols = matches("^(art_)?(sb_|var)"),
    names_to = c("condition", ".value"),
    names_pattern = "^(art_)?(sb_.*|var)"
  ) %>% 
  mutate(
    condition = if_else(condition == "art_", "artificial", "control"),
    procedure_fac = factor(procedure_id, levels = order_levels)
    
  )

plot_data_long %>% 
  ggplot(
    aes(x = procedure_fac, y = sb_estimate, color = condition)) +
  # line connecting control ↔ artificial
  geom_line(aes(group = procedure_id), color = "grey70") +
  # points
  geom_point(size = 3) +
  # manual colors
  scale_color_manual(
    values = c(
      "control" = "blue",
      "artificial" = "red"
    )
  ) +
  coord_flip() +
  labs(
    x = "Procedure ID",
    y = "Spearman-Brown Estimate",
    color = "Condition",
    title = "Effect of Artificial Condition on Reliability"
  ) +
  theme_minimal()

## Effsize plots ----
plot_data <- data %>% 
  filter(
    truth_rating_scale != "dichotomous"
  ) %>% 
  select(
    procedure_id,
    truth_rating_scale,
    n_statements,
    contains("d_")
  )

order_levels <- plot_data %>%
  arrange(d_estimate) %>%
  pull(procedure_id)

plot_data_long <- plot_data %>% 
  pivot_longer(
    cols = matches("^(art_)?(d_)"),
    names_to = c("condition", ".value"),
    names_pattern = "^(art_)?(d_.*)"
  ) %>% 
  mutate(
    condition = if_else(condition == "art_", "artificial", "control"),
    procedure_fac = factor(procedure_id, levels = order_levels)
    
  )

plot_data_long %>% 
  ggplot(
    aes(x = procedure_fac, y = d_estimate, color = condition)) +
  # line connecting control ↔ artificial
  geom_line(aes(group = procedure_id), color = "grey70") +
  # points
  geom_point(size = 3) +
  # manual colors
  scale_color_manual(
    values = c(
      "control" = "blue",
      "artificial" = "red"
    )
  ) +
  coord_flip() +
  labs(
    x = "Procedure ID",
    y = "Cohens d",
    color = "Condition",
    title = "Effect of Artificial Condition on Effect Size"
  ) +
  theme_minimal()


## Plot likert data ----
plot_data <- data %>% 
  filter(
    truth_rating_scale == "dichotomous",
    has_certainty == 1
  ) %>% 
  select(
    procedure_id,
    truth_rating_scale,
    n_statements,
    contains("d_")
  )

order_levels <- plot_data %>%
  arrange(d_estimate) %>%
  pull(procedure_id)

plot_data_long <- plot_data %>% 
  pivot_longer(
    cols = matches("^(art_)?(d_)"),
    names_to = c("condition", ".value"),
    names_pattern = "^(art_)?(d_.*)"
  ) %>% 
  mutate(
    condition = if_else(condition == "art_", "artificial", "control"),
    procedure_fac = factor(procedure_id, levels = order_levels)
  )

plot_data_long %>% 
  ggplot(
    aes(x = procedure_fac, y = d_estimate, color = condition)) +
  # line connecting control ↔ artificial
  geom_line(aes(group = procedure_id), color = "grey70") +
  # points
  geom_point(size = 3) +
  # manual colors
  scale_color_manual(
    values = c(
      "control" = "blue",
      "artificial" = "red"
    )
  ) +
  coord_flip() +
  labs(
    x = "Procedure ID",
    y = "Cohens d",
    color = "Condition",
    title = "Effect of Artificial Condition on Effectsize"
  ) +
  theme_minimal()

## Plot together data ----
plot_data <- data %>% 
  filter(truth_rating_scale == "likert" | has_certainty ==1) %>% 
  select(
    procedure_id,
    truth_rating_scale,
    n_statements,
    has_certainty,
    contains("sb_")
  )

order_levels <- plot_data %>%
  arrange(sb_estimate) %>%
  pull(procedure_id)

plot_data_long <- plot_data %>% 
  pivot_longer(
    cols = matches("^(art_)?(sb_|var)"),
    names_to = c("condition", ".value"),
    names_pattern = "^(art_)?(sb_.*|var)"
  ) %>% 
  mutate(
    condition = if_else(condition == "art_", "artificial", "control"),
    procedure_fac = factor(procedure_id, levels = order_levels)
    
  ) %>% 
  mutate(
    scale_type = case_when(
      truth_rating_scale == "dichotomous" & condition == "control" ~ "dichotomous",
      truth_rating_scale == "dichotomous" & condition == "artificial" ~ "likert",
      truth_rating_scale == "likert" & condition == "control" ~ "likert",
      truth_rating_scale == "likert" & condition == "artificial" ~ "dichotomous"
    )
  )

plot_data_long %>% 
  ggplot(
    aes(x = procedure_fac, y = sb_estimate, color = scale_type, shape = condition)) +
  # line connecting control ↔ artificial
  geom_line(aes(group = procedure_id), color = "grey70") +
  # points
  geom_point(size = 3) +
  # manual colors
  scale_color_manual(
    values = c(
      "dichotomous" = "blue",
      "likert" = "red"
    )
  ) +
  coord_flip() +
  labs(
    x = "Procedure ID",
    y = "Spearman-Brown Estimate",
    color = "Condition",
    title = "Effect of Artificial Condition on Reliability"
  ) +
  theme_minimal()
