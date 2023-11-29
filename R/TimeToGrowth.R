#' @name TimeToGrowth
#' @title Calculate Time To Growth
#' 
#' @description The Time to Growth (TTG) is the duration of time until a crop 
#'   advances a growth stage measured in minutes. The probability of growth is 
#'   the intersection of the independent events of a random tick being 
#'   assigned and growth occurring.
#'
#' @param growth_probability `numeric(1)` on the interval (0, 1). The 
#'   probability that a crop will advance a growth stage when assigned a
#'   random tick.
#' @param p `numeric(1)` on the interval (0, 1). The probability used for
#'   determining the quartile of the probability distribution. Used only 
#'   when `unit = "minute"`.
#' @param method `character(1)`. One of `c("median", "mean", "quantile")`.
#'   The method of calculation to be based on either the median, mean, or
#'   quantile of the probability distribution.
#' @param unit `character(1)`. One of `c("tick", "second", "minute", "hour")`.
#'   The unit of time for the metric. 
#' 

time_to_growth <- function(growth_probability,
                           p = 0.5,
                           method = c("median", "mean", "quantile"), 
                           unit = "minute"){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_numeric(x = growth_probability, 
                            len = 1, 
                            lower = 0, 
                            upper = 1, 
                            add = coll)

  checkmate::assert_numeric(x = p, 
                            len = 1, 
                            lower = 0, 
                            upper = 1, 
                            add = coll)

  method <- checkmate::matchArg(x = method, 
                                choices = c("median", "mean", "quantile"), 
                                .var.name = "method", 
                                add = coll)
  
  unit <- checkmate::matchArg(x = unit, 
                              choices = c("tick", "second", "minute", "hour"), 
                              .var.name = "unit", 
                              add = coll)

  checkmate::reportAssertions(coll)

  ticks <- tick_conversion(unit = unit)

  switch(method, 
         "mean" = (ttrt(method = "mean") / growth_probability) / ticks, 
         "median" = (-1 * ttrt(method = "median") / (log2(1 - growth_probability))) / ticks, 
         "quantile" = (ttrt(p = p, method = method) * qgeom(p, prob = growth_probability)) / ticks)
}
