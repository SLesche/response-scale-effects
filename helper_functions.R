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
