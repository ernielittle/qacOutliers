library(outliers)
library(ggplot2)

grubbs <- function(data) {
  grubbs_result <- grubbs.test(data)

  print(grubbs_result)

  outlier_value <- ifelse(grubbs_result$p.value < 0.05,
                          grubbs_result$outlier,
                          NA)

  df <- data.frame(value = data)
  df$outlier <- ifelse(data == outlier_value, "Outlier", "Normal")

  df$outlier <- as.factor(df$outlier)

  ggplot(df, aes(x = factor(1), y = value, color = outlier)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.2, size = 2) +
    labs(title = "Outlier Detection using Grubbs' Test",
         x = "Data",
         y = "Values") +
    theme_minimal() +
    scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
    guides(color = "none")
}

# Example usage:
data <- c(2, 3, 5, 6, 7, 8, 8, 9, 10, 15)
grubbs(data)







