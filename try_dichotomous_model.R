library(tidyverse)
library(acdcquery)

# load packages
library(gridExtra)
library(ggridges)
library(lemon)
library(patchwork)
library(viridis)
library(xtable)

# download_ted("data/")
check_ted("data/ted.db")
conn <- connect_to_db("data/ted.db")


all_argument <- list() %>% 
  add_argument(
    conn,
    "publication_id",
    "greater",
    "0"
  )

publications_overview <- query_db(
  conn,
  all_argument,
  "default",
  "publication_table"
)

study_overview <- query_db(
  conn,
  all_argument,
  "default",
  "study_table"
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

full_data <- query_db(
  conn,
  all_argument,
  c("default", "study_id", "publication_id", "statement_text", "statement_accuracy", 
    "statementset_id", "statementset_publication", "within_description", 
    "between_description", "repetition_time", "n_statements", "phase", 
    "truth_instructions", "repetition_instructions", "truth_instruction_timing", 
    "repetition_instruction_timing", "truth_rating_scale", "truth_rating_steps"),
  "observation_table"
)

references_table <- query_db(
  conn,
  all_argument,
  c("publication_id", "authors", "conducted", "peer_reviewed", "study_id", "n_participants"),
  "publication_table"
)

trial_nums <- full_data %>% 
  count(study_id) %>% 
  rename("n_trials" = "n")

references_table <- references_table %>% 
  left_join(., trial_nums) %>% 
  mutate(
    peer_reviewed = ifelse(peer_reviewed == 1, "Yes", "No")
  )

analysis_data <- full_data %>% 
  filter(phase == "test") %>% 
  filter(!is.na(repeated), !is.na(response)) %>% 
  mutate(
    truth_instruction_timing = ifelse(
      truth_instruction_timing == "",
      "none",
      truth_instruction_timing
    ),
    repetition_instruction_timing = ifelse(
      repetition_instruction_timing == "",
      "none",
      repetition_instruction_timing
    ),
    repetition_time_type = case_when(
      repetition_time <= 5 ~ "immediate",
      repetition_time > 5 & repetition_time <= 60 ~ "delay",
      repetition_time >= 60*24 ~ "next_day"
    )
  ) %>% 
  mutate(
    truth_instruction_timing = fct_relevel(
      factor(truth_instruction_timing),
      "none",
      "exposure", 
      "test",
      "both"
    ),
    repetition_instruction_timing = fct_relevel(
      factor(repetition_instruction_timing),
      "none",
      "exposure", 
      "test",
      "both"
    ),
    repetition_time_type = fct_relevel(
      factor(repetition_time_type),
      "immediate",
      "delay", 
      "next_day"
    ),
    repetition_sameday = ifelse(repetition_time < 60*24, 1, 0),
    repeated = repeated - 0.5
  )

has_complete_data <- analysis_data %>% 
  count(procedure_id, subject, repeated) %>% 
  count(procedure_id, subject) %>% 
  mutate(
    has_complete_data = ifelse(n == 2, 1, 0)
  )

analysis_data <- analysis_data %>% 
  left_join(
    ., has_complete_data
  ) %>% 
  filter(has_complete_data == 1) 

prop_true <- analysis_data %>% 
  group_by(statement_id) %>% 
  summarize(
    proportion_true = mean(response, na.rm = TRUE)
  )

analysis_data <- analysis_data %>% 
  left_join(prop_true)

prep <- analysis_data %>% 
  rename("sub" = subject,
         "cond" = repeated,
         "Y" = response,
         "item" = statement_id) %>% 
  group_by(procedure_id, truth_rating_scale) %>% 
  nest()

data_binary <- prep %>% 
  filter(truth_rating_scale == "dichotomous") %>% 
  # head(1) %>%
  pull(data)

data_ids_binary <- prep %>% 
  filter(truth_rating_scale == "dichotomous") %>% 
  # head(1) %>%
  pull(procedure_id)

data_scale <- prep %>% 
  filter(truth_rating_scale != "dichotomous") %>% 
  # head(2) %>% 
  pull(data)

data_ids_scale <- prep %>% 
  filter(truth_rating_scale != "dichotomous") %>% 
  # head(2) %>% 
  pull(procedure_id)

source("reliability_functions.R")
source("lesche_rel_functions.R")

## Compute Gamma ---------------------------------------------------------------

### with Method of Moments Formula (Rouder & Mehrvarz, 2024) -------------------
# will create NaNs when gamma^2 is negative
# results_mom <- lapply(data, mom)


### with Bayesian Hierarchical Modeling ----------------------------------------
data <- data_binary[[13]]
model <- fit_binary_model(data)
gamma <- compute_gamma(model, data)

print(gamma$gamma)
