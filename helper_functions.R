visualize_distribution <- function(vector){
  
}

get_distribution_stats <- function(vector){
  return(
    list(
      var = var(vector),
      sd = sd(vector),
      mean = mean(vector),
      min = min(vector),
      max = max(vector)
    )
  )
}

compute_subject_truth_effect <- function(data){
  result <- data %>% 
    group_by(subject, repeated) %>% 
    summarize(
      mean_truth = mean(response, na.rm = TRUE)
    ) %>% 
    pivot_wider(
      id_cols = "subject",
      values_from = "mean_truth",
      names_from = repeated,
      names_prefix = "repeated_"
    ) %>% 
    mutate(
      truth_effect = repeated_1 - repeated_0
    )
  
  return(result$truth_effect)
}

compute_reliability <- function(data){
  reliability <- splithalf(data = data,
                           outcome = "accuracy",
                           score = "difference",
                           halftype = "random",
                           permutations = 5000,
                           var.ACC = "response",
                           var.participant = "subject",
                           var.compare = "repeated",
                           compare1 = "0",
                           compare2 = "1",
                           average = "mean")
  
  return(
    list(
      estimates = reliability$estimates,
      final = reliability$final_estimates
    )
  )
}

compute_effsize <- function(data){
  average_data <- data %>% 
    group_by(subject, repeated) %>% 
    summarize(
      mean_truth = mean(response, na.rm = TRUE)
    )  %>% 
    pivot_wider(
      id_cols = "subject",
      values_from = "mean_truth",
      names_from = repeated,
      names_prefix = "repeated_"
    )
  
  effsize = effectsize::cohens_d(average_data$repeated_1, average_data$repeated_0, paired = TRUE)
  return(effsize)
}

compute_certainty_effsize <- function(data){
  
  if (all(is.na(data$certainty))){
    effsize = NA
  } else {
    average_data <- data %>% 
      group_by(subject, repeated) %>% 
      summarize(
        mean_truth = mean(certainty, na.rm = TRUE)
      )  %>% 
      pivot_wider(
        id_cols = "subject",
        values_from = "mean_truth",
        names_from = repeated,
        names_prefix = "certainty_"
      )
    
    effsize = effectsize::cohens_d(average_data$certainty_1, average_data$certainty_0, paired = TRUE)
  }

  return(effsize)
}


dichotomize_responses <- function(response){
  dich = case_when(
    response == 0.5 ~ 0.5,
    response < 0.5 ~ 0,
    response > 0.5 ~ 1
  )
}

# Function to maximum normalize a vector to range 0-1
max_normalize <- function(vec) {
  # Ensure the vector has more than one unique value to avoid division by zero
  if (length(unique(vec)) == 1) {
    warning("Vector has a single unique value; returning a vector of 0s")
    return(rep(0, length(vec)))
  }
  # Perform maximum normalization
  return((vec - min(vec, na.rm = TRUE)) / (max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE)))
}


likert_scale_response <- function(response, certainty){
  if (all(is.na(certainty)) == 1){
    likert_response = response
  } else {
    likert_response <- max_normalize((response - 0.5) * certainty)
  }
  
  return(likert_response)
}

transform_artificial_responses <- function(data, truth_rating_scale){
  if (truth_rating_scale == "dichotomous"){
    data$response = likert_scale_response(data$response, data$certainty)
  } else {
    data$response = dichotomize_responses(data$response)
  }
  
  return(data)
}

make_2x2_from_coefs <- function(model) {
  
  b <- coef(model)
  V <- vcov(model)
  
  b0 <- b["intrcpt"]
  b1 <- b["scale_typescale"]
  b2 <- b["conditionartificial"]
  b3 <- b["scale_typescale:conditionartificial"]
  
  X <- list(
    c(1,0,0,0),
    c(1,0,1,0),
    c(1,1,0,0),
    c(1,1,1,1)
  )
  
  est <- sapply(X, function(x) sum(x * b))
  
  se <- sapply(X, function(x) sqrt(t(x) %*% V %*% x))
  
  data.frame(
    scale_type = rep(c("dichotomous", "scale"), each = 2),
    condition  = rep(c("control", "artificial"), times = 2),
    estimate = est,
    se = se,
    ci.lb = est - 1.96 * se,
    ci.ub = est + 1.96 * se
  )
}

plot_sb_comparison <- function(data){
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
        truth_rating_scale == "dichotomous" & condition == "artificial" ~ "scale",
        truth_rating_scale == "likert" & condition == "control" ~ "scale",
        truth_rating_scale == "likert" & condition == "artificial" ~ "dichotomous",
        truth_rating_scale == "range" & condition == "control" ~ "scale",
        truth_rating_scale == "range" & condition == "artificial" ~ "dichotomous",
      )
    ) %>% mutate(
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
  
  improve_df <- plot_data_long %>%
    dplyr::select(procedure_id, condition, sb_estimate) %>%
    tidyr::pivot_wider(
      names_from = condition,
      values_from = sb_estimate
    ) %>%
    dplyr::mutate(
      diff = artificial - control,
      improve = diff > 0
    )
  
  star_df <- plot_data_long %>%
    dplyr::filter(condition == "artificial") %>%
    dplyr::left_join(
      improve_df %>% dplyr::select(procedure_id, improve, control),
      by = "procedure_id"
    ) %>%
    filter(improve == TRUE) %>% 
    mutate(
      sb_estimate = 0.95
    )%>% 
    mutate(
      scale_type = ifelse(truth_rating_scale.x == "dichotomous", "scale", "dichotomous")
    )
  
  plot_data_long %>% 
    ggplot(
      aes(x = procedure_fac, y = sb_estimate, color = scale_type, shape = condition)) +
    # line connecting control ↔ artificial
    geom_line(aes(group = procedure_id), color = "grey70") +
    # points
    geom_point(size = 5) +
    # manual colors
    scale_color_manual(
      values = c(
        "dichotomous" = dichotomous_color,
        "scale" = scale_color
      )
    ) +
    # scale_shape_manual(
    #   values = c(
    #     "control" = dichotomous_color,
    #     "artificial" = scale_color
    #   )
    # ) +
    geom_text(
      data = star_df,
      aes(x = procedure_fac, y = sb_estimate, color = ifelse(control < 0.2, "grey",scale_type)),
      label = "+",
      size = 7
    )+ 
    scale_x_discrete(guide = guide_axis(n.dodge = 2))+
    coord_flip() +
    labs(
      x = "Procedure ID",
      y = "Spearman-Brown Estimate",
      color = "Scale Type",
      shape = "Condition",
      # title = "Effect of Artificial Condition on Reliability"
    ) +
    ylim(-0.5, 1)+
    theme_minimal()+
    theme(
      text = element_text(family = "Times New Roman", colour = "black"),
      axis.text = element_text(family = "Times New Roman", size = 15, color = "black"),
      axis.title = element_text(family = "Times New Roman", size = 20, color = "black"),
      legend.text = element_text(family = "Times New Roman", size = 20),
      legend.title = element_text(family = "Times New Roman", size = 20)
    )
}

plot_d_comparison <- function(data){
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
        truth_rating_scale == "dichotomous" & condition == "artificial" ~ "scale",
        truth_rating_scale == "likert" & condition == "control" ~ "scale",
        truth_rating_scale == "likert" & condition == "artificial" ~ "dichotomous",
        truth_rating_scale == "range" & condition == "control" ~ "scale",
        truth_rating_scale == "range" & condition == "artificial" ~ "dichotomous",
      )
    ) %>% mutate(
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
  
  improve_df <- plot_data_long %>%
    dplyr::select(procedure_id, condition, d_estimate) %>%
    tidyr::pivot_wider(
      names_from = condition,
      values_from = d_estimate
    ) %>%
    dplyr::mutate(
      diff = artificial - control,
      improve = diff > 0
    )
  
  star_df <- plot_data_long %>%
    dplyr::filter(condition == "artificial") %>%
    dplyr::left_join(
      improve_df %>% dplyr::select(procedure_id, improve, control),
      by = "procedure_id"
    ) %>%
    filter(improve == TRUE) %>% 
    mutate(
      d_estimate = 1.4
    )%>% 
    mutate(
      scale_type = ifelse(truth_rating_scale.x == "dichotomous", "scale", "dichotomous")
    )
  
  
  plot_data_long %>% 
    ggplot(
      aes(x = procedure_fac, y = d_estimate, color = scale_type, shape = condition)) +
    # line connecting control ↔ artificial
    geom_line(aes(group = procedure_id), color = "grey70") +
    # points
    geom_point(size = 5) +
    # manual colors
    scale_color_manual(
      values = c(
        "dichotomous" = dichotomous_color,
        "scale" = scale_color
      )
    ) +
    geom_text(
      data = star_df,
      aes(x = procedure_fac, y = d_estimate, color = scale_type),
      label = "+",
      size = 7
    )+ 
    scale_x_discrete(guide = guide_axis(n.dodge = 2))+
    coord_flip() +
    labs(
      x = "Procedure ID",
      y = "Spearman-Brown Estimate",
      color = "Condition",
      # title = "Effect of Artificial Condition on Reliability"
    ) +
    theme_minimal()+
    ylim(-0.5, 1.5)+
    theme_minimal()+
    theme(
      text = element_text(family = "Times New Roman", colour = "black"),
      axis.text = element_text(family = "Times New Roman", size = 15, color = "black"),
      axis.title = element_text(family = "Times New Roman", size = 20, color = "black"),
      legend.text = element_text(family = "Times New Roman", size = 20),
      legend.title = element_text(family = "Times New Roman", size = 20)
    )
}
