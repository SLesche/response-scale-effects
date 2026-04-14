procedure_test = 93

test_s <- data %>% 
  filter(procedure_id == procedure_test) %>% 
  pull(data) %>% 
  .[[1]] %>% 
  group_by(subject, repeated) %>%
  summarize(mean_resp = mean(response), mean_cert = mean(certainty)) %>% 
  pivot_wider(
    id_cols = "subject",
    values_from = c("mean_resp", "mean_cert"),
    names_from = repeated,
    names_prefix = "repeated_"
  )

effectsize::cohens_d(test_s$mean_resp_repeated_1, test_s$mean_resp_repeated_0, paired = TRUE)
effectsize::cohens_d(test_s$mean_cert_repeated_1, test_s$mean_cert_repeated_0, paired = TRUE)

