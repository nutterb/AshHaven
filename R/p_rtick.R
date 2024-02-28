#' @name RandomTicks
#' @title Random Tick Mechanics
#' 
#' @description Functions for Basic Random Tick Mechanics
#' 
#' @inheritParams validate_common_farm_args
#' @inheritParams geom_quantile
#' @param method `character(1)`. One of `c("median", "mean", "quantile")`. 
#'   Permits the user control over whether the median, mean, or a quantile of
#'   Time to Random Tick is returned.
#'   
#' @details `p_rtick` returns the probability that a random tick is assigned
#'   to a block during a game tick.
#'
#' `ttrt` returns the Time to Random Tick, based on a geometric distribution.
#' 
#' @export

p_rtick <- function(f, 
                    fr = 0:n, 
                    random_tick_speed = getOption("randomTickSpeed", 3), 
                    B = getOption("subchuckSize", 16^3)){
  coll <- checkmate::makeAssertCollection()
  
  validate_common_farm_args(f = f, 
                            fr = fr, 
                            random_tick_speed = random_tick_speed, 
                            B = B, 
                            coll = coll)
  
  checkmate::reportAssertions(coll)
  
  dhyper(fr, f, B-f, random_tick_speed)
}

#' @rdname RandomTicks
#' @export

ttrt <- function(f, 
                 p = 0.5, 
                 method = c("median", "mean", "quantile"),
                 random_tick_speed = getOption("randomTickSpeed", 3), 
                 B = getOption("subchunkSize", 16^3)){
  coll <- checkmate::makeAssertCollection()
  
  method <- checkmate::matchArg(x = method, 
                                choices = c("median", "mean", "quantile"), 
                                .var.name = "method", 
                                add = coll)
  
  checkmate::assertNumeric(x = p, 
                           lower = 0, 
                           upper = 1, 
                           add = coll)
  
  validate_common_farm_args(f = f, 
                            random_tick_speed = random_tick_speed, 
                            B = B, 
                            coll = coll)
  
  checkmate::reportAssertions(coll)
  
  P_RTICK <- sum(p_rtick(f = f, fr = 1:3, random_tick_speed = random_tick_speed, B = B))
  
  switch(method, 
         "mean" = 1 / P_RTICK,
         "median" = -1 / (log2(1 - P_RTICK)), 
         "quantile" = geom_quantile(p, growth_probability = P_RTICK))
}
