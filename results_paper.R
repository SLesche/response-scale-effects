library(tidyverse)
library(splithalf)
library(acdcquery)
library(metafor)
source("helper_functions.R")

# Get data ----
data <- readRDS("data/reliability_data.Rdata")
conn <- connect_to_db("data/ted.db")

all_argument <- list() %>% 
  add_argument(
    conn,
    "publication_id",
    "greater",
    "0"
  )

procedure_data <- query_db(
  conn,
  all_argument %>% 
    add_argument(
      conn,
      "phase",
      "equal",
      "test"
    ),
  "default",
  "procedure_table"
)

data <- data %>% 
  mutate(
    n_participants = map_dbl(reliability, ~.x$final$n),
    scale_type = ifelse(truth_rating_scale == "dichotomous", "dichotomous", "scale")
  ) %>% 
  left_join(procedure_data)

## 1. overview table ----
overview_table_data <- data %>% 
  select(procedure_id,
         n_statements,
         n_participants,
         repetition_time,
         truth_rating_scale,
         contains("d_"),
         contains("sb_")
         ) %>% 
  mutate(
    across(where(is.numeric), ~round(., 2))
  ) %>% 
  mutate(
    d_control = paste0(d_estimate, " [", d_low, ", ", d_high, "]"),
    d_art = paste0(art_d_estimate, " [", art_d_low, ", ", art_d_high, "]"),
    rel_control = paste0(sb_estimate, " [", sb_low, ", ", sb_high, "]"),
    rel_art = paste0(art_sb_estimate, " [", art_sb_low, ", ", art_sb_high, "]"),
  ) %>% 
  select(
    procedure_id,
    n_participants,
    n_statements,
    repetition_time,
    truth_rating_scale,
    d_control,
    d_art,
    rel_control,
    rel_art
  )

## Density plot effsize and rel ----
# Should we add this?


## Average Rel by type ----
sb_data_long <- data %>% 
  select(
    procedure_id,
    repetition_time,
    truth_rating_scale,
    scale_type,
    n_statements,
    contains("sb_")
  ) %>% 
  pivot_longer(
    cols = matches("^(art_)?(sb_|var)"),
    names_to = c("condition", ".value"),
    names_pattern = "^(art_)?(sb_.*|var)"
  ) %>% 
  mutate(
    condition = if_else(condition == "art_", "artificial", "control"),
    )

# average_rel_by_scaletype <- sb_data_long %>% 
#   ungroup() %>% 
#   group_by(scale_type, condition) %>% 
#   summarize(
#     mean_rel = mean(sb_estimate, na.rm = TRUE)
#   )

sb_data_long <- sb_data_long %>%
  ungroup() %>% 
  mutate(
    scale_type = factor(
      scale_type,
      levels = c("dichotomous", "scale"),
      labels = c("dichotomous", "scale")
    ),
    condition = factor(
      condition,
      levels = c("control", "artificial"),
      labels = c("control", "artificial")
    )
  )

res_sb <- rma.mv(
  yi = sb_estimate,
  V = (sb_high - sb_low) / (2 * 1.96)^2,
  mods = ~ scale_type*condition,
  random = ~ 1 | procedure_id,
  data = sb_data_long
)

average_rel_by_scaletype <- make_2x2_from_coefs(res_sb)

## Average Effect by type ----
d_data_long <- data %>% 
  select(
    procedure_id,
    truth_rating_scale,
    repetition_time,
    scale_type,
    n_statements,
    contains("d_"),
    -contains("cert_")
  ) %>% 
  pivot_longer(
    cols = matches("^(art_)?(d_|var)"),
    names_to = c("condition", ".value"),
    names_pattern = "^(art_)?(d_.*|var)"
  ) %>% 
  mutate(
    condition = if_else(condition == "art_", "artificial", "control"),
  )

# average_effsize_by_scaletype <- d_data_long %>%
#   ungroup() %>%
#   group_by(scale_type, condition) %>%
#   summarize(
#     mean_rel = mean(d_estimate, na.rm = TRUE)
#   )

d_data_long <- d_data_long %>%
  ungroup() %>% 
  mutate(
    scale_type = factor(
      scale_type,
      levels = c("dichotomous", "scale"),
      labels = c("dichotomous", "scale")
    ),
    condition = factor(
      condition,
      levels = c("control", "artificial"),
      labels = c("control", "artificial")
    )
  )

res_d <- rma.mv(
  yi = d_estimate,
  V = (d_high - d_low) / (2 * 1.96)^2,
  mods = ~ scale_type*condition,
  random = ~ 1 | procedure_id,
  data = d_data_long
)

average_effsize_by_scaletype <- make_2x2_from_coefs(res_d)

## Plot effect of artificial change data rel ----
plot_data <- data %>% 
  filter(has_art == 1) %>% 
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
      truth_rating_scale == "likert" & condition == "artificial" ~ "dichotomous",
      truth_rating_scale == "range" & condition == "control" ~ "range",
      truth_rating_scale == "range" & condition == "artificial" ~ "dichotomous",
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
      "likert" = "red",
      "range" = "green"
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

## Plot effect of artificial change data effsize ----
plot_data <- data %>% 
  filter(has_art == 1) %>% 
  select(
    procedure_id,
    truth_rating_scale,
    n_statements,
    has_certainty,
    contains("d_")
  )

order_levels <- plot_data %>%
  arrange(d_estimate) %>%
  pull(procedure_id)

plot_data_long <- plot_data %>% 
  pivot_longer(
    cols = matches("^(art_)?(d_|var)"),
    names_to = c("condition", ".value"),
    names_pattern = "^(art_)?(d_.*|var)"
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
      truth_rating_scale == "likert" & condition == "artificial" ~ "dichotomous",
      truth_rating_scale == "range" & condition == "control" ~ "range",
      truth_rating_scale == "range" & condition == "artificial" ~ "dichotomous",
    )
  )

plot_data_long %>% 
  ggplot(
    aes(x = procedure_fac, y = d_estimate, color = scale_type, shape = condition)) +
  # line connecting control ↔ artificial
  geom_line(aes(group = procedure_id), color = "grey70") +
  # points
  geom_point(size = 3) +
  # manual colors
  scale_color_manual(
    values = c(
      "dichotomous" = "blue",
      "likert" = "red",
      "range" = "green"
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

## Comparing comparable studies: ----
# Here select some based on criteria suggested by Zajdler (same n_statements, same repetition_time)


## Below things go into an appendix if necessary ----

## Effect of n_statements ----
res_sb_n_statements <- rma.mv(
  yi = sb_estimate,
  V = (sb_high - sb_low) / (2 * 1.96)^2,
  mods = ~ n_statements,
  random = ~ 1 | procedure_id,
  data = sb_data_long %>% filter(condition == "control")
)

res_d_n_statements <- rma.mv(
  yi = d_estimate,
  V = (d_high - d_low) / (2 * 1.96)^2,
  mods = ~ n_statements,
  random = ~ 1 | procedure_id,
  data = d_data_long %>% filter(condition == "control")
)

## Effect of repetition_time ----
res_sb_repetition_time <- rma.mv(
  yi = sb_estimate,
  V = (sb_high - sb_low) / (2 * 1.96)^2,
  mods = ~ repetition_type,
  random = ~ 1 | procedure_id,
  data = sb_data_long %>% filter(condition == "control") %>% mutate(repetition_type = ifelse(repetition_time < 60*24, "sameday", "nextday"))
)

res_d_repetition_time <- rma.mv(
  yi = d_estimate,
  V = (d_high - d_low) / (2 * 1.96)^2,
  mods = ~ repetition_type,
  random = ~ 1 | procedure_id,
  data = d_data_long %>% filter(condition == "control") %>% mutate(repetition_type = ifelse(repetition_time < 60*24, "sameday", "nextday"))
)

## Maybe something else here?

#### OTHER PLOTS ARCHIVE -----
plot_data <- data %>% 
  # filter(
  #   truth_rating_scale != "dichotomous"
  # ) %>% 
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
    has_certainty == 1,
    has_art == 1,
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
    has_certainty == 1,
    has_art == 1
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
    cols = matches("^(art_|cert_)?(d_)"),
    names_to = c("condition", ".value"),
    names_pattern = "^(art_|cert_)?(d_.*)"
  ) %>% 
  mutate(
    condition = case_when(
      condition == "art_" ~ "artificial_likert",
      condition == "" ~ "control_truth",
      condition == "cert_" ~ "control_certainty"
    ),
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
      "control_truth" = "blue",
      "artificial_likert" = "red",
      "control_certainty" = "green"
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


