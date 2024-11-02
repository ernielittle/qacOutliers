#'@title Univariate Boxplot with Statistics
#'@description Generate a boxplot and return statistics from base R.
#'@param data A data frame.
#'@param x A character string naming the numeric variable.
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

