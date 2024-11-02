#'@title Univariate Outlier Detection with Boxplot
#'@description Generate a boxplot specifically for univariate outlier detection and return associated statistics from base R.
#'@param data A data frame containing the variable to be analyzed.
#'@param x A character string naming the numeric variable to assess for outliers.
#'@returns A list containing the ggplot object and boxplot statistics.
#'@import ggplot2
#'@export
#'@examples
#'uni_box(mtcars, "mpg")
#'uni_box(mtcars, "hp")

uni_box <- function(data, x) {
  if (!x %in% names(data)) stop(paste("The specified column", x, "does not exist in the data frame."))

  # Calculate boxplot stats using base R
  stats <- boxplot.stats(data[[x]])
  cat("Univarate Boxplot Statistics for", x, ":\n")
  print(stats)  # Print boxplot statistics

  # Create the ggplot boxplot
  p <- ggplot(data, aes_string(y = x)) +
    geom_boxplot(outlier.colour = "red") +
    ggtitle(paste("Univariate Boxplot of", x)) +
    theme_minimal()

  return(list(plot = p, stats = stats))
}

