library(tidyverse)
library(lme4)
library(acdcquery)
library(metafor)
source("helper_functions.R")

scale_color <- "#FF6B6B"
dichotomous_color <- "#4682B4"

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
  left_join(procedure_data, by = c("procedure_id", "n_statements")) %>%
  left_join(study_overview %>% select(-n_participants), by = c("truth_rating_scale", "study_id")) %>% 
  left_join(publications_overview, by = "publication_id")

## 1. overview table ----
overview_table_data <- data %>% 
  select(procedure_id,
         publication_id,
         authors,
         has_art,
         conducted,
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
  mutate(
    d_art = ifelse(has_art, d_art, "N/A"),
    rel_art = ifelse(has_art, rel_art, "N/A")
  ) %>% 
  select(
    publication_id,
    authors, 
    conducted,
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
plot_sb_comparison(data)
ggsave("markdown/images/rel_change_plot.jpg", plot_sb_comparison(data), width = 16, height = 9)

## Plot effect of artificial change data effsize ----
plot_d_comparison(data)
ggsave("markdown/images/eff_change_plot.jpg", plot_d_comparison(data), width = 16, height = 9)

## Comparing comparable studies: ----
# Here select some based on criteria suggested by Zajdler (same n_statements, same repetition_time)
plot_sb_comparison(
  data %>% 
    filter(
      n_statements > 40 & n_statements < 80,
      repetition_time < 60
    )
  )
ggsave("markdown/images/matched_rel_change_plot.jpg", plot_sb_comparison(
  data %>% 
    filter(
      n_statements > 40 & n_statements < 80,
      repetition_time < 60
    )
), width = 16, height = 9)


plot_d_comparison(
  data %>% 
    filter(
      n_statements > 40 & n_statements < 80,
      repetition_time < 60
    )
)

ggsave("markdown/images/matched_eff_change_plot.jpg", plot_d_comparison(
  data %>% 
    filter(
      n_statements > 40 & n_statements < 80,
      repetition_time < 60
    )
), width = 16, height = 9)

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

## Interaction effects ----
res_sb_int <- rma.mv(
  yi = sb_estimate,
  V = (sb_high - sb_low) / (2 * 1.96)^2,
  mods = ~ repetition_type*truth_rating_scale,
  random = ~ 1 | procedure_id,
  data = sb_data_long %>% filter(condition == "control") %>% mutate(repetition_type = ifelse(repetition_time < 60*24, "sameday", "nextday"))
)

res_sb_int <- rma.mv(
  yi = sb_estimate,
  V = (sb_high - sb_low) / (2 * 1.96)^2,
  mods = ~ repetition_location*truth_rating_scale,
  random = ~ 1 | procedure_id,
  data = sb_data_long %>%
    filter(condition == "control") %>%
    mutate(repetition_type = ifelse(repetition_time < 60*24, "sameday", "nextday")) %>% 
    left_join(procedure_data, by = "procedure_id") %>% left_join(study_overview) %>% 
    mutate(truth_rating_scale = ifelse(truth_rating_scale == "dichotomous", "dichotomous", "scale"))
)

## Certainty Effects ----
raw_data <- data.table::fread("data/analysis_data.csv") %>% 
  mutate(direction = ifelse(response < 0.5, '"False" statements', '"True" statements')) %>% 
  mutate(art_certainty = abs(response - 0.5)) 

dichotomous_data <- raw_data %>% 
  filter(procedure_id %in% data$procedure_id) %>% 
  filter(
    truth_rating_scale == "dichotomous",
  ) %>% 
  mutate(certainty = (certainty - 1) / 5)

# Pre-compute per-procedure means (for individual lines)
dichotomous_proc_means_certainty_response <- dichotomous_data %>%
  filter(!is.na(certainty)) %>% 
  mutate(
    procedure_id = factor(procedure_id),
    repeated = ifelse(repeated == 0, "new", "repeated")
  ) %>%
  group_by(procedure_id, direction, repeated) %>%
  summarize(
    mean_cert = mean(certainty), .groups = "drop",
    mean_response = mean(response)
  )

# Pre-compute overall average + 95% CI (for the average line + error bars)
dichotomous_overall_means <- dichotomous_data %>%
  mutate(repeated = ifelse(repeated == 0, "new", "repeated")) %>%
  filter(!is.na(certainty)) %>% 
  group_by(direction, repeated) %>%
  summarize(
    mean_cert  = mean(certainty, na.rm = TRUE),
    se_cert         = sd(certainty) / sqrt(n()),
    ci_low_cert     = mean_cert - 1.96 * se_cert,
    ci_high_cert    = mean_cert + 1.96 * se_cert,
    .groups    = "drop"
  )

# Plot
dichotomous_impact_on_certainty_plot <- ggplot() +
  # Individual procedure lines (light, in background)
  geom_line(
    data = dichotomous_proc_means_certainty_response,
    aes(x = repeated, y = mean_cert,
        color = procedure_id, group = procedure_id),
    alpha = 0.6
  ) +
  # Overall average line
  geom_line(
    data = dichotomous_overall_means,
    aes(x = repeated, y = mean_cert, group = 1),
    color = "black", linewidth = 1.2
  ) +
  # Error bars around the average
  geom_errorbar(
    data = dichotomous_overall_means,
    aes(x = repeated, ymin = ci_low_cert, ymax = ci_high_cert),
    color = "black", width = 0.1, linewidth = 1
  ) +
  facet_wrap(~direction) +
  labs(
    x     = "Statement Repetition Status",
    y     = "Mean certainty rating",
    # title = "Certainty ratings by exposure type and direction"
  ) +
  theme_minimal()+
  theme(legend.position = "none")+
  theme(
    text = element_text(family = "Times New Roman", colour = "black"),
    axis.text = element_text(family = "Times New Roman", size = 15, color = "black"),
    axis.title = element_text(family = "Times New Roman", size = 20, color = "black"),
    legend.text = element_text(family = "Times New Roman", size = 20),
    legend.title = element_text(family = "Times New Roman", size = 20),
    strip.text = element_text(family = "Times New Roman", size = 20)
    
  )

ggsave("markdown/images/dichotomous_impact_on_certainty_plot.jpg", dichotomous_impact_on_certainty_plot, width = 16, height = 9)

dichotomous_table_certainty_by_repetition <- dichotomous_data %>% 
  filter(!is.na(certainty)) %>% 
  group_by(
    # procedure_id,
    repeated,
  ) %>% 
  summarize(mean_certainty = mean(certainty))

cert_model_dichotomous <- lmerTest::lmer(certainty ~ repeated*response + (1 | subject) + (1 | procedure_id), data = dichotomous_data)
summary(cert_model_dichotomous)

## For likert data ----
likert_data <- raw_data %>% 
  filter(procedure_id %in% data$procedure_id) %>% 
  filter(
    truth_rating_scale == "likert"
  )

# Pre-compute per-procedure means (for individual lines)
likert_proc_means_certainty_response <- likert_data %>%
  mutate(
    procedure_id = factor(procedure_id),
    repeated = ifelse(repeated == 0, "new", "repeated")
  ) %>%
  group_by(procedure_id, direction, repeated) %>%
  summarize(
    mean_cert = mean(art_certainty), .groups = "drop",
    mean_response = mean(response)
    )

# Pre-compute overall average + 95% CI (for the average line + error bars)
likert_overall_means <- likert_data %>%
  mutate(repeated = ifelse(repeated == 0, "new", "repeated")) %>%
  group_by(direction, repeated) %>%
  summarize(
    mean_cert  = mean(art_certainty),
    se_cert         = sd(art_certainty) / sqrt(n()),
    ci_low_cert     = mean_cert - 1.96 * se_cert,
    ci_high_cert    = mean_cert + 1.96 * se_cert,
    mean_response  = mean(response),
    se_response         = sd(response) / sqrt(n()),
    ci_low_response     = mean_response - 1.96 * se_response,
    ci_high_response    = mean_response + 1.96 * se_response,
    .groups    = "drop"
  )

# Plot
likert_impact_on_response_plot <- ggplot() +
  # Individual procedure lines (light, in background)
  geom_line(
    data = likert_proc_means_certainty_response,
    aes(x = repeated, y = mean_response,
        color = procedure_id, group = procedure_id),
    alpha = 0.6
  ) +
  # Overall average line
  geom_line(
    data = likert_overall_means,
    aes(x = repeated, y = mean_response, group = 1),
    color = "black", linewidth = 1.2
  ) +
  # Error bars around the average
  geom_errorbar(
    data = likert_overall_means,
    aes(x = repeated, ymin = ci_low_response, ymax = ci_high_response),
    color = "black", width = 0.1, linewidth = 1
  ) +
  facet_wrap(~direction) +
  labs(
    x     = "Statement Repetition Status",
    y     = "Mean truth rating",
    # title = "Certainty ratings by exposure type and direction"
  ) +
  theme_minimal()+
  theme(legend.position = "none")+
  theme(
    text = element_text(family = "Times New Roman", colour = "black"),
    axis.text = element_text(family = "Times New Roman", size = 15, color = "black"),
    axis.title = element_text(family = "Times New Roman", size = 20, color = "black"),
    legend.text = element_text(family = "Times New Roman", size = 20),
    legend.title = element_text(family = "Times New Roman", size = 20),
    strip.text = element_text(family = "Times New Roman", size = 20)
  )

ggsave("markdown/images/likert_impact_on_response_plot.jpg", likert_impact_on_response_plot, width = 16, height = 9)

# Plot
likert_impact_on_certainty_plot <- ggplot() +
  # Individual procedure lines (light, in background)
  geom_line(
    data = likert_proc_means_certainty_response,
    aes(x = repeated, y = mean_cert,
        color = procedure_id, group = procedure_id),
    alpha = 0.6
  ) +
  # Overall average line
  geom_line(
    data = likert_overall_means,
    aes(x = repeated, y = mean_cert, group = 1),
    color = "black", linewidth = 1.2
  ) +
  # Error bars around the average
  geom_errorbar(
    data = likert_overall_means,
    aes(x = repeated, ymin = ci_low_cert, ymax = ci_high_cert),
    color = "black", width = 0.1, linewidth = 1
  ) +
  facet_wrap(~direction) +
  labs(
    x     = "Statement Repetition Status",
    y     = "Mean artificial certainty rating",
    # title = "Certainty ratings by exposure type and direction"
  ) +
  theme_minimal()+
  theme(legend.position = "none")+
  theme(
    text = element_text(family = "Times New Roman", colour = "black"),
    axis.text = element_text(family = "Times New Roman", size = 15, color = "black"),
    axis.title = element_text(family = "Times New Roman", size = 20, color = "black"),
    legend.text = element_text(family = "Times New Roman", size = 20),
    legend.title = element_text(family = "Times New Roman", size = 20),
    strip.text = element_text(family = "Times New Roman", size = 20)
    
  )

ggsave("markdown/images/likert_impact_on_certainty_plot.jpg", likert_impact_on_certainty_plot, width = 16, height = 9)


likert_table_certainty_by_repetition <- likert_data %>% 
  group_by(
    # procedure_id,
    repeated,
  ) %>% 
  summarize(mean_certainty = mean(art_certainty))


likert_table_response_by_repetition <- likert_data %>% 
  group_by(
    repeated,
    direction
  ) %>% 
  summarize(mean_resp = mean(response))

likert_model_certainty_by_repetition <- lmerTest::lmer(art_certainty ~ repeated*direction + (1 + repeated | subject) + (1 | procedure_id), data = likert_data)
# likert_model_response_by_repetition <- lmerTest::lmer(response ~ repeated*direction + (1 | subject) + (1 | procedure_id), data = likert_data)
