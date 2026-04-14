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

data <- prep %>% 
  mutate(
    has_certainty = map(
      data, ~ifelse(
        mean(is.na(.x$certainty)) == 1,
        0,
        1
      )
    )
  ) %>% 
  mutate(
    art_data = map2(data, truth_rating_scale, transform_artificial_responses)
  ) %>% 
  mutate(
    truth_effect = map(data, compute_subject_truth_effect),
    art_truth_effect = map(art_data, compute_subject_truth_effect)
  ) %>% 
  mutate(
    stats = map(truth_effect, get_distribution_stats),
    art_stats = map(art_truth_effect, get_distribution_stats),
  )

true_dich <- data %>% 
  filter(truth_rating_scale == "dichotomous") %>% 
  unnest(data)

true_likert <- data %>% 
  filter(truth_rating_scale == "likert") %>% 
  unnest(data)

art_dich <- data %>% 
  filter(truth_rating_scale == "likert") %>% 
  unnest(art_data)

art_likert <- data %>% 
  filter(truth_rating_scale == "dichotomous") %>% 
  unnest(art_data)

rm(data)
rm(prep)
gc()

true_dich_model <- lme4::glmer(
  response ~ repeated + 
    (1+ repeated || subject) + (1 | procedure_id) + (1 | statement_id),
  family = binomial,
  data = true_dich
)

art_dich_model <- lme4::glmer(
  response ~ repeated + 
    (1+ repeated || subject) + (1 | procedure_id) + (1 | statement_id),
  family = binomial,
  data = art_dich
)


true_likert_model <- lme4::lmer(
  response ~ repeated + 
    (1+ repeated || subject) + (1 | procedure_id) + (1 | statement_id),
  data = true_likert
)

art_likert_model <- lme4::lmer(
  response ~ repeated + 
    (1+ repeated || subject) + (1 | procedure_id) + (1 | statement_id),
  data = art_likert
)



## In new data without random effec tof procedure
true_dich_var_sub_proc <- c(lme4::VarCorr(true_dich_model)$subject)
art_likert_var_sub_proc <- c(lme4::VarCorr(art_likert_model)$subject)

true_likert_var_sub_proc <- c(lme4::VarCorr(true_likert_model)$subject)
art_dich_var_sub_proc <- c(lme4::VarCorr(art_dich_model)$subject)


## From old data with random effect of procedure
true_dich_var_sub_proc <- c(lme4::VarCorr(true_dich_model)$subject, lme4::VarCorr(true_dich_model)$procedure_id)
art_likert_var_sub_proc <- c(lme4::VarCorr(art_likert_model)$subject, lme4::VarCorr(art_likert_model)$procedure_id)

true_likert_var_sub_proc <- c(lme4::VarCorr(true_likert_model)$subject, lme4::VarCorr(true_likert_model)$procedure_id)
art_dich_var_sub_proc <- c(lme4::VarCorr(art_dich_model)$subject, lme4::VarCorr(art_dich_model)$procedure_id)

# In Artificial Dich
# > 0.985 / 0.147
# [1] 6.70068
# 
# In Artificial Likert
# > 0.011907 / 0.000957
# [1] 12.44201
# 
# In True Dich
# > 0.3854 / 0.0895
# [1] 4.306145
# 
# In True Likert
# > 0.01905 / 0.00437
# [1] 4.359268
