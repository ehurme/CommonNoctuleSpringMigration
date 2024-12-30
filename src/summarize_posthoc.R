summarize_posthoc <- function(test_data, units = NA, round = 0,
                              adjust = 0, divide = 1) {
  require(dplyr)
  require(tidyr)
  # Extract the median zero column value
  zero_column_median <- test_data$post_hoc$median_zero_column[1]
  zero_n <- test_data$post_hoc$n_zero_column[1]

  # Create the summarized data frame
  summary_data <- test_data$post_hoc %>%
    mutate(
      p_value = ifelse(p.value < 0.0001, "< 0.0001", round(p.value, 4)),#format(p.value, scientific = TRUE)),
      median = paste0(round((median_column + adjust)/divide, round), units),
      stat = as.character(round(statistic, round)),
      n = as.character(n_column)
    ) %>%
    dplyr::select(level, median, stat, p_value, n)

  # Add the zero column values
  zero_data <- data.frame(
    level = 0,
    median = paste0(round((zero_column_median + adjust) / divide, round), units),
    stat = NA,
    p_value = NA,
    n = as.character(zero_n),
    stringsAsFactors = FALSE
  )

  # Combine and order the data
  summary_data <- bind_rows(summary_data[1:2, ], zero_data, summary_data[3:nrow(summary_data), ])

  # Convert all columns to character
  summary_data <- summary_data %>%
    mutate(across(everything(), as.character))

  # Transpose the data to make the level column the title
  transposed_summary <- summary_data %>%
    pivot_longer(cols = -level, names_to = "type", values_to = "value") %>%
    pivot_wider(names_from = level, values_from = value)

  return(transposed_summary)
}

# Example usage with temp_test
temp_test <- list(
  kw_test = list(statistic = 100.76, df = 4, p.value = 2.2e-16),
  post_hoc = tibble::tibble(
    statistic = c(576, 938, 3995, 3738),
    p.value = c(4.72e-12, 1.05e-8, 1.48e-5, 4.91e-4),
    method = c("Wilcoxon", "Wilcoxon", "Wilcoxon", "Wilcoxon"),
    alternative = c("two.sided", "two.sided", "two.sided", "two.sided"),
    comparison = c("t2m_-48 vs t2m_0", "t2m_-24 vs t2m_0", "t2m_24 vs t2m_0", "t2m_48 vs t2m_0"),
    median_column = c(282, 284, 286, 286),
    median_zero_column = c(285, 285, 285, 285),
    column_hour = c(-48, -24, 24, 48),
    zero_hour = c(0, 0, 0, 0),
    level = c(-2, -1, 1, 2),
    label = c("***", "***", "***", "***"),
    n_column = c(30, 30, 30, 30),
    n_zero_column = c(30, 30, 30, 30)
  )
)
