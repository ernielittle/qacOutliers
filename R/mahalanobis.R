#'@title Quick scatter plot
#'@description Create scatter plot with linear line of best fit, correlation, and p-value
#'@export
#'@param data a data frame
#'@param x a numeric variable
#'@param y a numeric variable
#'@returns a tibble with n, mean, and standard deviation
#'@import ggplot2
#'@import Routliers
#'@examples
#'qscatter(mtcars, wt, hp)
#'

qscatter <- function(data,x,y){

  #making them into names so they can be in the title
  xname <- as.character(substitute(x))
  yname <- as.character(substitute(y))

  #getting subtitle information
  r <- cor.test(data[[xname]], data[[yname]])

  p <- format.pval(r$p.value, 3)
  cor <- round(r$estimate, 3)

  ggplot(data=data)+
    geom_smooth(aes(x={{x}}, y={{y}}), method="lm", se=F,
                color='cornflowerblue', linetype=5, formula=y~x)+
    geom_point(aes(x={{x}}, y={{y}}))+
    theme_minimal()+
    labs(title = paste("Relationship between", xname, "and", yname),
         subtitle = paste("r = ", cor, "p < ", p))
}
