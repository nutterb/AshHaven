#' @name geom_quantile
#' @title Quantiles for Geometric Distribution
#' 
#' @description the `qgeom` function returns an integer value, which can 
#'   produce some less than helpful results (such as 10% of crops will be
#'   mature after 0 seconds). This approximates the geometric distribution 
#'   quantiles on a continuous basis.
#'  
#' @param growth_probability `numeric(1)` on the interval (0, 1). The 
#'   probability that a crop will advance a growth stage when assigned a
#'   random tick.
#' @param p `numeric(1)` on the interval (0, 1). The probability used for 
#'   calculating the quantile via [qgeom()].   
#' 
#' @export

geom_quantile <- function(p, growth_probability){
  f <- function(x, p, growth_probability){
    p - (1 - (1 - growth_probability)^x)
  }
  
  stats::uniroot(f, 
                 interval = c(0, 1e8), 
                 growth_probability = growth_probability, 
                 p = p)$root
}

# Unexported

geom_mean <- function(p){
  1 / p
}

geom_median <- function(p){
  -1 / (log2(1 - p))
}
