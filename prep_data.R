library(tidyverse)
library(acdcquery)

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
    # repeated = repeated - 0.5
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

data.table::fwrite(analysis_data, "data/analysis_data.csv")
