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
#'data(Attacks)
#'SOC <- rowMeans(Attacks[,c("soc1r","soc2r","soc3r","soc4","soc5","soc6","soc7r", "soc8","soc9","soc10r","soc11","soc12","soc13")])
#'HSC <- rowMeans(Attacks[,22:46])
#'multiOutliers(data = data.frame(SOC, HSC), method="mahalanbois")
#'multiOutliers(mtcars, disp, cyl, method="mahalanobis")
#'multiOutliers(mtcars, method="LoF")

multiOutliers <- function(data, x, y, method, minPts, ...){
  #add other methods as people finish them here

  if(method=="LoF"){
    # Check if data is a matrix or data frame and convert if necessary
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data should be a matrix or data frame.")
    }

    # Remove any non numeric data
    data <- data[sapply(data, is.numeric)]

    # Check if there are enough points for the LOF calculation
    if (nrow(data) <= minPts) {
      stop("Number of data points must be greater than minPts.")
    }

    # Calculate the LoF scores
    lof_scores <- dbscan::lof(data, minPts = minPts)

    # Append the LOF scores as a new column in the data frame
    data_with_lof <- data.frame(ID = 1:nrow(data), data, LOF_Score = lof_scores)


    # Return the data frame with IDs, original data, and LOF scores
    return(data_with_lof)
  }

  if(method=="mahalanobis"){
    library(dplyr)
    library(Routliers)

    #taking only numeric data
    numeric_data <-select_if(data, is.numeric)

    #make this into a matrix
    mat <- as.matrix(numeric_data)

    #run matrix on function and store results
    results <- outliers_mahalanobis(x=mat)
    index <- results$outliers_pos
    values <- results$outliers_val
  }

  if (method == "kNN") {
    if (!is.matrix(data)) {
      data <- as.matrix(data)
    }

    threshold <- 0.95
    k <- 5

    # Calculate pairwise distances
    dist_matrix <- as.matrix(dist(data))

    # Get k-nearest neighbors for each point (excluding self-distance of 0)
    knn_scores <- apply(dist_matrix, 1, function(row) {
      sort(row, partial = k + 1)[2:(k + 1)]
    })

    # Calculate the average distance to the k-nearest neighbors
    avg_knn_distances <- rowMeans(knn_scores)

    # Determine the outliers based on the threshold
    cutoff <- quantile(avg_knn_distances, threshold)
    outliers <- which(avg_knn_distances > cutoff)

    # Return results
    return(list(outliers = outliers, scores = avg_knn_distances))
  }
  else stop("Method supplied must be kNN, mahalanobis, iForest, or LoF.")
}

