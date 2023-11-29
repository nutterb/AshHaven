#' @name ExpectedCropOutput
#' @title Expected Crop Output
#' 
#' @description The Expected Crop Output (ECO) is a measure of how much crop 
#'   is produced by a single block of farm per minute.
#'   
#' @inheritParams TimeToGrowth
#' @inheritParams TimeToMaturity
#' @param yield `numeric(1)`. The number of times returned by harvesting
#'   a crop. This may be an average and need not be a whole number.
#'  
#' @export 

expected_crop_output <- function(yield, 
                                 growth_probability, 
                                 stages = 1,
                                 p = 0.5,
                                 method = c("median", "mean", "quantile"), 
                                 unit = "minute"){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_numeric(x = yield, 
                            len = 1,
                            lower = 0,
                            add = coll)
  
  checkmate::assert_numeric(x = growth_probability, 
                            len = 1, 
                            lower = 0, 
                            upper = 1, 
                            add = coll)
  
  checkmate::assert_integerish(x = stages, 
                               len = 1, 
                               lower = 1, 
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
  
  ttm <- time_to_maturity(growth_probability = growth_probability, 
                          stages = stages, 
                          p = p, 
                          method = method, 
                          unit = unit)
  
  yield / ttm
}
