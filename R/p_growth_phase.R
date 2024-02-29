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
#' @param g `integerish(1)` on the interval [0, fr]. The number of growth events
#'   observed.
#' @param fr `integerish(1)` on the interval [0, random_tick_speed]. 
#'   The number of blocks of farm receiving a random tick.
#' @param f `integerish(1)`. The number of blocks of farmland.
#' @param random_tick_speed `integerish(1)`. the random tick speed (3 by default)
#' @param B `integerish(1)`. The number of blocks in a subchunk (16^3 by default)
#' 
#' @export

p_growth_phase <- function(p_growth,
                           g, 
                           fr,
                           f,
                           margin = c("none", "fr", "g"),
                           random_tick_speed = getOption("randomTickSpeed", 3), 
                           B = getOption("subchunkSize", 16^3)){
  coll <- checkmate::makeAssertCollection()
  
  validate_common_farm_args(f = f, 
                            random_tick_speed = random_tick_speed, 
                            B = B, 
                            fr = NULL, 
                            coll = coll)
  
  checkmate::assertNumeric(x = p_growth, 
                           lower = 0, 
                           upper = 1, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertIntegerish(x = g, 
                              lower = 0, 
                              upper = random_tick_speed, 
                              len = 1,
                              add = coll)
  
  checkmate::assertIntegerish(x = fr, 
                              lower = 0, 
                              upper = random_tick_speed, 
                              len = 1, 
                              add = coll)
  
  margin <- checkmate::matchArg(x = margin, 
                                choices = c("none", "fr", "g"), 
                                add = coll)
  
  checkmate::reportAssertions(coll)
  
  SampleSpace <- 
    p_growth_samplespace(p_growth = p_growth, 
                         f = f, 
                         random_tick_speed = random_tick_speed, 
                         B = B)

  numerator <- which(SampleSpace$fr == fr & SampleSpace$g == g)
  numerator <- SampleSpace$prob[numerator]
  
  if (margin == "none"){
    denominator <- 1
  } else if (margin == "g"){
    denominator <- which(SampleSpace$g == g)
    denominator <- sum(SampleSpace$prob[denominator])
  } else {
    denominator <- which(SampleSpace$fr == fr)
    denominator <- sum(SampleSpace$prob[denominator])
  }
  
  numerator / denominator
}


# Unexported --------------------------------------------------------

p_growth_samplespace <- function(p_growth, f, 
                                 random_tick_speed = getOption("randomTickSpeed", 3), 
                                 B = getOption("subchunkSize", 16^3)){
  SampleSpace <- 
    expand.grid(g = 0:random_tick_speed, 
                fr = 0:random_tick_speed, 
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
