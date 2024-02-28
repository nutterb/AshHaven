#' @name p_growth_phase 
#' @title Calculate the probability of crops experiencing a growth phase within a tick
#' 
#' @description As a farm increases in size, the probability of a growth event 
#'   somewhere in the farm changes. The probability is dependent on the 
#'   number of blocks assigned to the farm. Furthermore, there may be anywhere
#'   from zero to three blocks assigned a random tick, and anywhere from 
#'   zero to three of those randomly ticked blocks may experience growth. 
#'   The probability is a joint probability distribution of the hypergeometric
#'   distribution (for the number of blocks receiving a random tick)
#'   and a binomial distribution (for the number of randomly ticked blocks
#'   that experience growth).
#'   
#' @param p_growth `numeric(1)` on the interval (0, 1). The probability of the
#'   crop experiencing growth when it is assigned a random tick.
#' @param f `integerish(1)`. The number of blocks of farmland.
#' @param g `integerish`. Defaults to `0:max(f_r)`. 
#' @param fr `integerish`. Defaults to `0:rts`.
#' @param random_tick_speed `integerish(1)`. the random tick speed (3 by default)
#' @param B `integerish(1)`. The number of blocks in a subchunk (16^3 by default)
#' 
#' @export

p_growth_phase <- function(p_growth,
                           f,
                           g = 0:fr, 
                           fr = 0:random_tick_speed, 
                           random_tick_speed = getOption("randomTickSpeed", 3), 
                           B = getOption("subchunkSize", 16^3)){
  coll <- checkmate::makeAssertCollection()
  
  validate_common_farm_args(f = f, 
                            random_tick_speed = random_tick_speed, 
                            B = B, 
                            fr = fr, 
                            coll = coll)
  
  checkmate::assertNumeric(x = p_growth, 
                           lower = 0, 
                           upper = 1, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = g, 
                              lower = 0, 
                              upper = max(fr), 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  SampleSpace <- 
    expand.grid(g = g, 
                fr = fr, 
                p_growth = p_growth, 
                f = f)
  
  p_rtick <- 
    mapply(p_rtick, 
           f = SampleSpace$f, 
           fr = SampleSpace$fr, 
           MoreArgs = list(random_tick_speed = random_tick_speed, 
                           B = B), 
           SIMPLIFY = TRUE)
  
  p_growth_phase <- dbinom(x = SampleSpace$g, 
                           size = SampleSpace$fr, 
                           prob = SampleSpace$p_growth)
  

  SampleSpace$prob <- p_rtick * p_growth_phase
  SampleSpace$prob <- SampleSpace$prob / sum(SampleSpace$prob)
  
  SampleSpace
}
