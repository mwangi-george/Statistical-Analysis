##-----------t test-----------------##

# Conduct a t-test on diff
test_results <- t.test(
  sample_dem_data$diff,
  alternative = "greater",
  nu = 0
)

# See the results
test_results

# Conduct a paired t-test on dem_percent_12 and dem_percent_16
test_results <- t.test(
  sample_dem_data$dem_percent_12,
  sample_dem_data$dem_percent_16,
  alternative = "greater",
  paired = T
)


# See the results
test_results