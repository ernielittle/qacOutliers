# Grubbs method for unioutlier with package requirement

grubbs_method <- function(data, alpha = 0.05) {

  require(outliers)

  outliers <- c()  # to store detected outliers
  data_copy <- data  # work on a copy of the data

  repeat {
    # Apply Grubbs' test to the data
    test_result <- outliers::grubbs.test(data_copy, type = 10)  # testing for one outlier

    # Extract test statistic and critical value
    G_stat <- abs(test_result$statistic)
    critical_value <- abs(test_result$parameter["critical value"])

    # Check if the test statistic exceeds the critical value
    if (G_stat > critical_value) {
      # Identify and record the outlier
      outlier <- data_copy[which.max(abs(data_copy - mean(data_copy)))]
      outliers <- c(outliers, outlier)
      # Remove the outlier from the data
      data_copy <- data_copy[data_copy != outlier]
    } else {
      # Exit loop when no more outliers are found
      break
    }
  }

  # Return results as a list
  list(outliers = outliers, data_without_outliers = data
