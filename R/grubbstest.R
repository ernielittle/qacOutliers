library(outliers)
library(ggplot2)

grubbs <- function(data) {
  grubbs_result <- grubbs.test(data)

  print(grubbs_result)

  # Determine if there is an outlier based on p-value
  if (grubbs_result$p.value < 0.05) {
    outlier_value <- grubbs_result$outlier
  } else {
    outlier_value <- NULL
  }

  # Create a dataframe to store values and their outlier status
  df <- data.frame(value = data)

  # Assign outlier status for labeling
  df$outlier <- ifelse(data %in% outlier_value, "Outlier", "Normal")

  df$outlier <- as.factor(df$outlier)

  # Create a boxplot with all points colored blue
  ggplot(df, aes(x = factor(1), y = value)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(color = "blue", size = 3) +  # Set all points to blue
    labs(title = "Outlier Detection using Grubbs' Test",
         x = "Data",
         y = "Values") +
    theme_minimal() +
    theme(axis.text.x = element_blank())  # Hide x-axis text for clarity
}

# Example usage:
data <- c(2, 3, 5, 6, 7, 8, 8, 9, 10, 16)
grubbs(data)







