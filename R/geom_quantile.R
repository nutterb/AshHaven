#' @name geom_quantile
#' @title Quantiles for Geometric Distribution
#' 
#' @description the `qgeom` function returns an integer value, which can 
#'   produce some less than helpful results (such as 10% of crops will be
#'   mature after 0 seconds). This approximates the geometric distribution 
#'   quantiles on a continuous basis.
#'   
#' @inheritParams TimeToGrowth
#' 
#' @export

geom_quantile <- function(p, growth_probability){
  f <- function(x, p, growth_probability){
    p - (1 - (1 - growth_probability)^x)
  }
  
  uniroot(f, 
          interval = c(0, 1e8), 
          growth_probability = growth_probability, 
          p = p)$root
}

