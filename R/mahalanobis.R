#'@title Multivariate outlier detection
#'@description Create scatter plot with linear line of best fit, correlation, and p-value
#'@export
#'@param data a data frame
#'@param x a numeric variable
#'@param y a numeric variable
#'@returns a tibble with n, mean, and standard deviation
#'@import ggplot2
#'@import Routliers
#'@import dplyr
#'@examples
#'multiOutliers(mtcars)
#'

multiOutliers <- function(data, x, y, method="mahalanobis", ...){
  if(method=="mahalanobis"){
    #select just the rows given by the user
    subset <- select(data, {{x}}, {{y}})

    #make this into a matrix
    mat <- as.matrix(subset)

    #run matrix on function and store results
    results <- outliers_mahalanobis(x=mat)
    print(results)
  }
}
