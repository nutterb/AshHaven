#' @name GrowthProbability
#' @title Calculating Growth Probability
#' 
#' @description The probability of growth given a random tick is an important
#'   part of calculating metrics related to minecraft farming. The probability
#'   can be found by scouring source code, or sometimes can be found in sources
#'   like the Minecraft Wiki. Frequently, however, the actual growth 
#'   probability is not stated, but the "average time to growth" is. These
#'   functions calculate the growth probability for mean and median estimates
#'   of time to growth. 
#'
#' @param ttg `numeric(1)`. The stated  time to growth
#' @param method `character(1)`. The method of calculation. Most Minecraft 
#'   resources will report "average time" when using the median. Players should
#'   confirm the calculations make sense before utilizing the results.
#' @param unit `character(1)`. The unit of measure for the published 
#'   time to growth. 
#'   
#' @export

growth_probability <- function(ttg, 
                               method = c("median", "mean"),
                               unit = c("tick", "second", "minute", "hour")){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_numeric(x = ttg, 
                            len = 1, 
                            add = coll)
  
  method <- checkmate::matchArg(x = method, 
                                choices = c("median", "mean"), 
                                .var.name = "method", 
                                add = coll)
  
  unit <- checkmate::matchArg(x = unit, 
                              choices = c("tick", "second", "minute", "hour"), 
                              .var.name = "unit", 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  tick_convert <- tick_conversion(unit)
  
  switch(method, 
         "mean" = ttrt(method = "mean") / (tick_convert * ttg), 
         "median" = 1 - 2 ^ (-ttrt()/ (ttg)))
}

.growth_probability_median <- function(ttg){
  f <- function(gp, m) m - (-1 / (log2(1 - p)) - 1)
  
  stats::uniroot(f, 
                 interval = c(0.001, .999),
                 m = ttg)$root
}
