library(tidyverse)
library(acdcquery)
library(lme4)
source("helper_functions.R")

scale_color <- "#FF6B6B"
dichotomous_color <- "#4682B4"

# Get data ----
data <- readRDS("data/reliability_data.Rdata")
raw_data <- data.table::fread("data/analysis_data.csv")

data <- data %>% 
  filter(
    truth_rating_scale == "dichotomous",
    has_art == 1
  )

raw_data <- raw_data %>% 
  filter(procedure_id %in% unique(data$procedure_id))

example <- 91

raw_data %>% 
  # filter(procedure_id == example)
  group_by(
    # procedure_id,
    repeated,
    response
  ) %>% 
  summarize(mean_cert = mean(certainty))

model <- lmer(certainty ~ repeated + response + repeated:response + (1 | subject), data = raw_data)
summary(model)

# For likert data ----
data <- readRDS("data/reliability_data.Rdata")
raw_data <- data.table::fread("data/analysis_data.csv")

data <- data %>% 
  filter(
    truth_rating_scale == "likert"
  )

raw_data <- raw_data %>% 
  filter(procedure_id %in% unique(data$procedure_id))

raw_data %>% 
  # filter(procedure_id == example) %>% 
  mutate(direction = ifelse(response < 0.5, "false", "true")) %>% 
  group_by(
    # procedure_id,
    repeated,
    direction
  ) %>% 
  summarize(mean_resp = mean(response))

raw_data %>% 
  # filter(procedure_id == example) %>% 
  mutate(direction = ifelse(response < 0.5, "false", "true")) %>% 
  mutate(certainty = ifelse(direction == "true", response - 0.5, 0.5 - response)) %>% 
  group_by(
    # procedure_id,
    repeated,
  ) %>% 
  summarize(mean_certainty = mean(certainty))

model <- lmer(abs(response - 0.5) ~ repeated + (1 | subject), data = raw_data %>% 
                mutate(direction = ifelse(response < 0.5, "false", "true")))
summary(model)

raw_data %>% 
  # filter(procedure_id == example) %>% 
  mutate(direction = ifelse(response < 0.5, "false", "true")) %>% 
  mutate(certainty = ifelse(direction == "true", response - 0.5, 0.5 - response)) %>% 
  ggplot(
    aes(
      x = response,
      fill = factor(repeated)
    )
  )+
  geom_density(alpha = 0.5)+
  facet_wrap(~direction)
