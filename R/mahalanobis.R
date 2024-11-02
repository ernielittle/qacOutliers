#'@title Multivariate outlier detection
#'@description Identifies multivariate outliers
#'@export
#'@param data a data frame
#'@param x a numeric variable
#'@param y a numeric variable
#'@param method character, supplies the method to be used for outlier detection
#'@returns indices of detected outliers, if any
#'@import ggplot2
#'@import Routliers
#'@import dplyr
#'@examples
#'multiOutliers(mtcars)
#'

multiOutliers <- function(data, x, y, method="mahalanobis", ...){
  #add other methods as people finish them here

  if(method=="mahalanobis"){

    #create error messaging here for non-numeric variables

    #select just the rows given by the user
    subset <- select(data, {{x}}, {{y}})

    #make this into a matrix
    mat <- as.matrix(subset)

    #run matrix on function and store results
    results <- outliers_mahalanobis(x=mat)
    print(results)
  }
  else{
    stop("Method supplied must be kNN, mahalanobis, iForest, or LoF.")
  }
}
