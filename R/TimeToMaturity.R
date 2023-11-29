#' @name TimeToMaturity
#' @title Time to Crop Maturation
#' 
#' @description Time to Maturity (TTM) is the duration of time between when 
#'   it is planted and when it reaches a mature state and is ready to harvest. 
#'   Crops may encounter several stages of growth before maturity–such as wheat, 
#'   which encounters eight growth stages.
#'   
#' @inheritParams TimeToGrowth
#' @param stages `integerish(1)`. The number of growth stages a crop requires
#' before becoming mature.
#'   

time_to_maturity <- function(growth_probability, 
                             stages = 1,
                             p = 0.5,
                             method = c("median", "mean", "quantile"), 
                             unit = "minute"){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_numeric(x = growth_probability, 
                            len = 1, 
                            lower = 0, 
                            upper = 1, 
                            add = coll)
  
  checkmate::assert_integerish(x = stages, 
                               len = 1, 
                               lower = 1, 
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
  
  if (method == "median") p <- 0.5
  
  ticks <- tick_conversion(unit = unit)
  
  switch(method, 
         "mean" = time_to_growth(growth_probability, method = "mean", unit = unit) * stages,
         "median" = time_to_growth(growth_probability, method = "median", unit = unit) * stages, 
         "quantile" = time_to_growth(growth_probability, p, method = "quantile", unit = unit) * stages)
}
