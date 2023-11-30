#' @name TickConverion
#' @title Convert Ticks to Other Time Units
#' 
#' @description The fundamental unit of time in Minecraft is the tick. 
#'   Each tick lasts 0.05 seconds. Players normally interpret game mechanics
#'   in terms of seconds, minutes, or hours. 
#'   
#' @inheritParams TimeToGrowth
#' 
#' @export

tick_conversion <- function(unit = c("tick", "second", "minute", "hour")){
  coll <- checkmate::makeAssertCollection()
  
  unit <- checkmate::matchArg(x = unit, 
                              choices = c("tick", "second", "minute", "hour"), 
                              .var.name = "unit", 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  switch(unit, 
         "tick"   = 1, 
         "second" = 20, 
         "minute" = 1200, 
         "hour"   = 72000)
}