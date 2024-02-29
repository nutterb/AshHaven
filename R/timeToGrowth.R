#' @name TimeToGrowth
#' @title Time To Growth
#' 
#' @description Estimate the time to a growth stage event for a crop in a farm
#'   of a specified size.
#'   
#' @inheritParams validate_common_farm_args
#' @inheritParams geom_quantile
#' @inheritParams RandomTicks
#' 
#' @export

ttg <- function(p_growth, 
                f, 
                p = 0.5,
                method = c("median", "mean", "quantile"),
                random_tick_speed = getOption("randomTickSpeed", 3), 
                B = getOption("subchunkSize", 16^3)){
  
  coll <- checkmate::makeAssertCollection()
  
  validate_common_farm_args(f = f, 
                            random_tick_speed = random_tick_speed, 
                            B = B, 
                            coll = coll)
  
  method <- checkmate::matchArg(x = method, 
                                choices = c("median", "mean", "quantile"), 
                                .var.name = "method", 
                                add = coll)
  
  checkmate::assertNumeric(x = p, 
                           lower = 0, 
                           upper = 1, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  SampleSpace <- p_growth_samplespace(p_growth = p_growth, 
                                      f = f, 
                                      random_tick_speed = random_tick_speed, 
                                      B = B)

  SampleSpace <- SampleSpace[SampleSpace$g > 0, ]
  

  prob <- sum(SampleSpace$prob)
  
  switch(method, 
         "mean" = geom_mean(prob),
         "median" = geom_median(prob), 
         "quantile" = geom_quantile(p, growth_probability = prob))
}
