
perform_non_parametric_tests <- function(df, env_name) {
  require(dplyr)
  require(tidyr)
  require(broom)

  # Identify relevant columns
  columns <- names(df)[grepl(paste0(env_name, "_"), names(df))]
  zero_column_name <- paste0(env_name, "_0")

  # Reshape data for analysis
  long_data <- df %>%
    dplyr::select(all_of(c(columns, zero_column_name))) %>%
    pivot_longer(cols = everything(), names_to = "time_shift", values_to = "value")

  # Perform Kruskal-Wallis test
  kw_test <- kruskal.test(value ~ time_shift, data = long_data)

  # Results list
  results <- list(kw_test = kw_test)

  # If significant, perform post-hoc analysis
  #if (kw_test$p.value < 0.05) {
  post_hoc_results <- purrr::map_dfr(setdiff(columns, zero_column_name),
                                     ~wilcoxon_test(data = df, col = .x,
                                                    zero_column = zero_column_name,
                                                    env_name = env_name))
  post_hoc_results <- post_hoc_results %>%
    mutate(level = column_hour / 24,
           label = case_when(
             p.value > 0.05 ~ " ",
             p.value <= 0.05 & p.value > 0.01 ~ "*",
             p.value <= 0.01 & p.value > 0.001 ~ "**",
             p.value <= 0.001 ~ "***"
           ))
  results$post_hoc <- post_hoc_results
  #}

  return(results)
}

wilcoxon_test <- function(data, col, zero_column, env_name = env_name) {
  # Prepare data for Wilcoxon test
  test_data <- data %>%
    dplyr::select(all_of(c(col, zero_column))) %>%
    na.omit()

  # Conduct Wilcoxon signed-rank test for paired comparisons
  test_result <- wilcox.test(test_data[[col]], test_data[[zero_column]], paired = TRUE)

  # Extract hours from the column names
  column_hour <- as.numeric(gsub(paste0(env_name, "_"), "", col))
  zero_hour <- as.numeric(gsub(paste0(env_name, "_"), "", zero_column))

  # Tidy results and add medians and non-NA counts
  tidy_result <- broom::tidy(test_result)
  tidy_result$comparison <- paste(col, "vs", zero_column)
  tidy_result$median_column <- median(test_data[[col]], na.rm = TRUE)
  tidy_result$median_zero_column <- median(test_data[[zero_column]], na.rm = TRUE)
  tidy_result$column_hour <- column_hour
  tidy_result$zero_hour <- zero_hour
  tidy_result$n_column <- sum(!is.na(test_data[[col]]))
  tidy_result$n_zero_column <- sum(!is.na(test_data[[zero_column]]))

  return(tidy_result)
}
