#' @name TickMechanics
#' @title Tick Mechanics
#' 
#' @description Functions for Basic Tick Mechanics
#' 
#' @param random_tick_speed `integerish(1)`. The `randomTickSpeed` setting for
#'   the world. 
#' @param p `numeric(1)` on the interval (0, 1). The probability used for 
#'   calculating the quantile via [qgeom()].
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

p_rtick <- function(random_tick_speed = getOption("randomTickSpeed", 3)){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = random_tick_speed, 
                               len = 1, 
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  random_tick_speed / (16^3)
}

#' @rdname TickMechanics
#' @export

ttrt <- function(random_tick_speed = getOption("randomTickSpeed", 3), 
                 p = 0.5,
                 method = c("median", "mean", "quantile")){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = random_tick_speed, 
                               len = 1, 
                               lower = 0, 
                               add = coll)
  
  method <- checkmate::matchArg(x = method, 
                                choices = c("median", "mean", "quantile"), 
                                .var.name = "method", 
                                add = coll)
  
  checkmate::reportAssertions(coll)
  
  P_RTICK <-  p_rtick(random_tick_speed)
  
  switch(method, 
         "mean" = 1 / P_RTICK,
         "median" = -1 / (log2(1 - P_RTICK)), 
         "quantile" = geom_quantile(p, growth_probability = P_RTICK))
}
