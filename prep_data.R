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
  head(1) %>%
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

## Compute Gamma ---------------------------------------------------------------

### with Method of Moments Formula (Rouder & Mehrvarz, 2024) -------------------
# will create NaNs when gamma^2 is negative
# results_mom <- lapply(data, mom)


### with Bayesian Hierarchical Modeling ----------------------------------------
posterior_variances_binary <- lapply(data_binary, sample_variances_binary_simulation)

# sample posterior variances
if (file.exists("model/posterior_variances_binary.rds")) {
  posterior_variances_binary <- readRDS("model/posterior_variances_binary.rds")
  message("Loaded posterior variances from RDS file.")
} else {
  posterior_variances_binary <- lapply(data_binary, sample_variances_binary)
  saveRDS(posterior_variances_binary, "model/posterior_variances_binary.rds")
  message("Sampled posterior variances and saved them to an RDS file.")
}

# sample posterior variances
if (file.exists("model/posterior_variances_scale.rds")) {
  posterior_variances_scale <- readRDS("model/posterior_variances_scale.rds")
  message("Loaded posterior variances from RDS file.")
} else {
  posterior_variances_scale <- lapply(data_scale, sample_variances_scale)
  saveRDS(posterior_variances_scale, "model/posterior_variances_scale.rds")
  message("Sampled posterior variances and saved them to an RDS file.")
}


# compute gamma using posterior variance samples
results <- lapply(posterior_variances_binary, compute_gamma)

# convert to tibble
dat <- tibble(
  sigma_w = as.numeric(map(results, "sigma_w")),
  sigma_b = as.numeric(map(results, "sigma_b")),
  gamma_square = as.numeric(map(results, "gamma_square")),
  gamma = as.numeric(map(results, "gamma")),
  gamma_distribution = map(results, "gamma_distribution")
) |>
  mutate(dataset = row_number()) |>
  select(dataset, sigma_w, sigma_b, gamma, gamma_square, gamma_distribution)
dat$lower <- numeric(length(results))
dat$upper <- numeric(length(results))
dat$L <- numeric(length(results))
dat$N <- numeric(length(results))
for (i in 1:length(results)) {
  dat$lower[i] = quantile(dat$gamma_distribution[[i]], probs = .025)
  dat$upper[i] = quantile(dat$gamma_distribution[[i]], probs = .975)
  dat$L[i] <- as.numeric(round(mean(table(data_binary[[i]]$sub, data_binary[[i]]$cond)))) # add trial number per condition (repeated vs new) to dataframe
  dat$N[i] <- length(unique(data_binary[[i]]$sub))
}

dat$procedure_id <- data_ids_binary

### adding truth status to the Model -------------------------------------------
# Maybe put this in again?

## Visualization ---------------------------------------------------------------

### Preparations ---------------------------------------------------------------

plot_theme <- function(base_size = 12, base_family = 'sans') {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid = element_blank(),
      panel.grid.major = element_line(color = "gray95", linewidth = 0.05),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      text = element_text(family = base_family, size = base_size),
      plot.title = element_text(face = "bold", size = base_size * 1.2, hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size * 0.9),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black",
                                linewidth = 0.2),
      axis.ticks.length = unit(0.2, "cm"),
      legend.title = element_text(hjust = 0.5),
      legend.title.position = "top",
      legend.text = element_text(size = base_size*0.75)
    )
}
theme_set(plot_theme())

# functions
# calculate mean truth effect in one dataset
mean_te <- function(df) {
  df <- aggregate(df$Y ~ df$cond, FUN = mean)
  mean_te <- df[2,2] - df[1,2]
  return(mean_te)
}

# read in additional information from manually coded data
overview <- procedure_data %>% 
  left_join(., study_overview) 

vizdata <- dat %>% 
  left_join(., overview) %>% 
  left_join(., publications_overview) %>% 
  select(authors, conducted, study_id, n_participants, everything())

### Reliability, Gamma, and Trial Size -----------------------------------------

# simulate some data
gamma <- rep(c(.1, .3, .5, .6, .7, 1), each = 200)
L <- c(1:200)
reldata <- data.frame(gamma, L)
reldata$rel <- rel(reldata)

# make the plot
breaks <- c(1:10, seq(20, 200, by = 10))
labels <- ifelse(breaks %in% c(1, 5, 10, 100, 200), breaks, "") 

ggplot(data = reldata,
       aes(x = L, y = rel, color = fct_rev(as.factor(gamma)))) +
  scale_x_continuous(trans='log10',
                     breaks = breaks,
                     minor_breaks = NULL,
                     labels = labels) + 
  geom_line(linewidth = 1) +
  coord_capped_cart(left = "both", 
                    bottom = "both") +
  theme(legend.title = element_text(hjust=0.5),
        legend.margin = margin(t = 10, b = 5, l = 7, r = 7),
        axis.title.y = element_text(margin = margin(r = 8), angle = 0, vjust = 0.5),
  ) +
  labs(x = "Trial Size (L)",
       y = expression(rho),
       color = expression(gamma)) +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "darkgrey") +
  scale_color_viridis_d(option = "turbo", begin=0.2, end=0.9)

ggsave("reliability.pdf", dpi = 300, width = 12, height = 7, units = "cm")


### Posterior Gamma Distributions ----------------------------------------------

# restructure data to access posterior gamma distributions
gamma_distr_long <- vizdata |>
  unnest(gamma_distribution) |>
  rename(value = gamma_distribution) |>
  mutate(procedure_id = factor(procedure_id))

# make the plot
ggplot() +
  geom_density_ridges(data=gamma_distr_long, 
                      aes(x = value, y = procedure_id), 
                      alpha = 0.5, 
                      scale = 1.1, 
                      rel_min_height = 0.01) +  
  geom_errorbar(data = vizdata, 
                aes(y = factor(procedure_id), x = gamma, xmin = lower, xmax = upper), 
                width = 0.25, 
                linewidth = 0.1) +
  geom_point(data = vizdata, 
             aes(x = gamma, y = factor(procedure_id), size = L, color = factor(study_id))) + 
  scale_x_continuous(limits = c(0, 1.2),
                     breaks = seq(0, 1.2, by = 0.2)) +
  labs(x = expression(gamma),
       y = "Dataset") +
  coord_capped_cart(left = "both", 
                    bottom = "both") +
  theme(legend.background = element_rect(
    color = "black",
    linewidth = 0.2,
    linetype = "solid"
  ),
  legend.position = "inside",
  legend.position.inside = c(0.805, 0.32),
  ) +
  scale_size_continuous(name = "Trial size (L)", range = c(1, 4)) + 
  guides(color = "none") + 
  scale_color_viridis_d(option = "turbo", begin=0.2, end=0.9)

ggsave("gamma_overview.pdf", dpi = 300, width = 15, height = 24, units = "cm")


### Observed Reliability -------------------------------------------------------
vizdata$reliability <- rel(vizdata)

ggplot(data = vizdata, 
       aes(x = gamma, y = reliability)) +
  geom_point(aes(size = L), 
             color = 'black',
             shape = 21, 
             fill = viridis(100, option = "turbo")[20], 
             stroke = 0.1) +
  coord_capped_cart(left = "both", 
                    bottom = "both") +
  theme(
    axis.title.y = element_text(margin = margin(r = 8), vjust = 0.5),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "inside",
    legend.position.inside = c(0.837, 0.30),
    legend.margin = margin(t = 10, b = 5, l = 7, r = 7)
  ) +
  labs(x = expression(gamma),
       y = "Expected Reliability",
       color = "L") +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "darkgrey") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 1))

ggsave("obs_rel.pdf", dpi = 300, width = 10, height = 9, units = 'cm')


### Covariation gamma and overall effect ----

te <- lapply(data, mean_te)
te <- tibble(
  dataset = data_ids,
  mean_te = as.numeric(unlist(te))
)

tedata <- merge(te, vizdata)
cor(tedata$mean_te, tedata$gamma)
c <- round(cor(tedata$mean_te, tedata$gamma), 2)*100

ggplot(data = tedata, 
       aes(x = mean_te, y = gamma)) +
  geom_point(size = 4,
             color = 'black',
             shape = 21, 
             fill = viridis(100, option = "turbo")[20], 
             stroke = 0.1) +
  geom_smooth(method = "lm", se = TRUE, 
              color = "black", linewidth = 0.1) +
  scale_y_continuous(
    limits = c(0, 1),  
    breaks = seq(0, 1, by = 0.25)
  ) +
  scale_x_continuous(
    limits = c(-0.1, 0.6),  
    breaks = seq(-0.1, 0.6, by = 0.1)
  ) +
  coord_capped_cart(left = "both", 
                    bottom = "both") +
  theme(
    axis.title.y = element_text(margin = margin(r = 8), angle = 0, vjust = 0.5),
    axis.text.y = element_text(margin = margin(r = 5))
  ) +
  labs(x = "Mean Truth Effect",
       y = expression(gamma)) +
  annotate("text", 
           x = -0.05, y = 0.75, 
           label = paste0("r = .", c),
           hjust = 0, vjust = 0,
           size = 4, family = "sans")

ggsave("mean_te.pdf", dpi = 300, width = 10, height = 10, units = 'cm')


### Plot sigma_w and sigma_b distributions -------------------------------------

p_sigma_b <- ggplot(data = vizdata, 
                    aes(x = sigma_b)) +
  geom_histogram(bins = 12, 
                 fill = viridis(100, option = "turbo")[20], 
                 color = "black", 
                 size = 0.1) +
  coord_capped_cart(left = "both", 
                    bottom = "both") +
  theme(
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text.y = element_text(margin = margin(r = 5))
  ) +
  scale_y_continuous(
    limits = c(0, 12),  
    breaks = seq(0, 12, by = 2)
  ) +
  labs(y = "Frequency",
       x = expression(sigma * phantom("0")[b]))

p_sigma_w <- ggplot(data = vizdata, 
                    aes(x = sigma_w)) +
  geom_histogram(bins = 12, 
                 fill = viridis(100, option = "turbo")[20], 
                 color = "black", 
                 size = 0.1) +
  coord_capped_cart(left = "both", 
                    bottom = "both") +
  theme(
    axis.text.y = element_text(margin = margin(r = 5))
  ) +
  scale_y_continuous(
    limits = c(0, 12),  
    breaks = seq(0, 12, by = 2)
  ) +
  labs(x = expression(sigma * phantom("0")[w]),
       y = "")

p_gamma <- ggplot(data = vizdata, 
                  aes(x = gamma)) +
  geom_histogram(bins = 12, 
                 fill = viridis(100, option = "turbo")[20], 
                 color = "black", 
                 size = 0.1) +
  coord_capped_cart(left = "both", 
                    bottom = "both") +
  theme(
    axis.text.y = element_text(margin = margin(r = 5))
  ) +
  scale_y_continuous(
    limits = c(0, 12),  
    breaks = seq(0, 12, by = 2)
  ) +
  labs(x = expression(gamma * phantom("0")),
       y = "")

p_sigma_b + p_sigma_w + p_gamma
ggsave("sigmaandgamma.pdf", dpi = 300, height = 5, width = 15, units = 'cm')


vizdata %>% 
  group_by(truth_rating_scale) %>%
  summarize(
    mean_gamma = mean(gamma),
    mean_b = mean(sigma_b),
    mean_w = mean(sigma_w)
  )

vizdata %>% 
  select(procedure_id, truth_rating_scale, sigma_w, sigma_b, gamma) %>% 
  pivot_longer(
    cols = -c(procedure_id, truth_rating_scale)
  ) %>% 
  ggplot(
    aes(
      x = value,
      color = truth_rating_scale,
      fill = truth_rating_scale
    )
  )+
  geom_density(alpha = 0.5) +
  facet_wrap(~name)
