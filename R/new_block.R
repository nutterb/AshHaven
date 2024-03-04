#' @name new_block
#' @title Create New Minecraft Block
#' 
#' @description Farmable blocks may have multiple growth stages before
#'   reaching maturity. This object type provides methods for 
#'   monitoring growth stages, maturity, and advancing toward maturity
#'   when assigned a random tick. 
#'   
#' @param type `character(1)`. Label for the type of block. 
#' @param is_farm `logical(1)`. Indicates if the block is being monitored
#'   as part of a farm.
#' @param growth_probability `numeric(1)` on the interval of (0, 1). The 
#'   probability the crop advances a growth stage when it is assigned 
#'   a random tick. 
#' @param current_growth_stage `integerish(1)`. Non-negative value storing 
#'   the growth stage of the crop. 
#' @param max_growth_stage `integerish(1)`. Non-negative value storing 
#'   the growth stage at which the crop reaches maturity.
#'
#' @return Returns an object of class `minecraft_block` with the following
#'   methods.
#'   
#' * `get_type()`: Returns the type attribute. 
#' * `set_type(new_type)`: Sets the type attribute. `new_type` must be `character(1)`.
#' * `is_farm()`: Returns the status of the block as part of the farm.
#' * `set_farm(state)`: Sets the `is_farm` property of the block. `state` must be `logical(1)`
#' * `grow_prob()`: Returns the growth probability of the crop.
#' * `set_grow_prob(p)`: Sets the growth probability. `p` must be `numeric(1)` on the interval (0, 1)
#' * `growth_stage()`: Returns the current growth stage of the block.
#' * `set_growth_stage(gs)`: Sets the growth stage to the value of `gs`. `gs` must be `integerish(1)` and non-negative.
#' * `advance_growth_stage(i)`: Adds the value of `i` to the current growth stage
#' * `max_growth_stage()`: Returns the maximum growth stage of the crop.
#' * `set_max_growth_stage(max)`: Sets the maximum growth stage of the crop. `max` must be `integerish(1)` and non-negative.
#' * `is_mature()`: Returns a `logical` indicating if the crop has reached maturity.
#' * `assign_random_tick()`: Attempts to advance the growth stage of the crop with a probabilty of success equal to `growth_probability`.
#' * `random_tick_count()`: Returns the number of times the block has been assigned a random tick. 
#' * `just_grew()`: Returns a `logical`. Used for convenience of data collection. Subchunk tick functions will set this property and use it to count how many randomly ticked chunks grew.
#' * `set_just_grew(state)`: Used to set the value of the `just_grew` property. `state` must be a `logical(1)`.
#' 
#' @export

new_block <- function(type = "none", 
                      is_farm = FALSE,
                      growth_probability = 0, 
                      current_growth_stage = 0, 
                      max_growth_stage = 1){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = type, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertLogical(x = is_farm, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertNumeric(x = growth_probability, 
                           len = 1, 
                           lower = 0, 
                           upper = 1,
                           add = coll)
  
  checkmate::assertIntegerish(x = current_growth_stage, 
                              len = 1, 
                              lower = 0, 
                              add = coll)
  
  checkmate::assertIntegerish(x = max_growth_stage, 
                              len = 1, 
                              add = coll)
  
  # Private object properties ---------------------------------------
  
  this_type <- type
  this_is_farm <- is_farm
  this_p_grow <- growth_probability
  this_current_gs <- current_growth_stage
  this_max_gs <- max_growth_stage
  this_rt_count <- 0
  this_just_grew <- FALSE

  # Object Methods --------------------------------------------------
  
  # Type of block.
  get_type <- function() this_type
  set_type <- function(new_type){
    checkmate::assertCharacter(x = new_type, 
                               len = 1)
    this_type <<- new_type
  } 
  
  # Farming properties
  is_farm <- function() this_is_farm
  set_farm <- function(state = TRUE){
    checkmate::assertLogical(x = state, 
                             len = 1)
    this_is_farm <<- state  
  } 
  
  p_grow <- function() this_p_grow
  set_p_grow <- function(p){
    checkmate::assertNumeric(x = p, 
                             lower = 0, 
                             upper = 1, 
                             len = 1)
    this_p_grow <<- p
  } 
  
  # Growth stages
  current_growth_stage = function() this_current_gs
  set_growth_stage = function(gs){
    checkmate::assertIntegerish(x = gs, 
                                len = 1, 
                                lower = 0)
    this_current_gs <<- gs
  }
  advance_growth_stage = function(i = 1){ 
    checkmate::assertIntegerish(x = i, 
                                lower = 0,
                                len = 1)
    this_current_gs <<- min(c(this_current_gs + i, 
                              this_max_gs))
  }
  
  max_growth_stage = function() this_max_gs
  set_max_growth_stage = function(max){
    checkmate::assertIntegerish(x = max, 
                                len = 1, 
                                lower = 0)
    this_max_gs <<- max
  }
  
  # Mature
  is_mature = function() this_current_gs == this_max_gs
  
  # Assign Random Tick
  assign_random_tick <- function(){
    this_rt_count <<- this_rt_count + 1
    
    did_grow <- sample(x = 0:1, 
                       size = 1, 
                       prob = c(1 - this_p_grow, this_p_grow))
    
    this_just_grew <<- as.logical(did_grow)
    
    this_current_gs <<- min(c(this_current_gs + did_grow, 
                              this_max_gs))
  }
  
  just_grew <- function() this_just_grew
  set_just_grew <- function(state = FALSE){
    checkmate::assertLogical(x = state, 
                             len = 1)
    this_just_grew <<- state
  }
    
  # Object Return ---------------------------------------------------
  structure(
    list(get_type = get_type, 
         set_type = set_type, 
         
         is_farm = is_farm, 
         set_farm = set_farm, 
         
         grow_prob = p_grow, 
         set_grow_prob = set_p_grow,
         
         growth_stage = current_growth_stage, 
         set_growth_stage = set_growth_stage,
         advance_growth_stage = advance_growth_stage, 
         
         max_growth_stage = max_growth_stage,
         set_max_growth_stage = set_max_growth_stage,
         is_mature = is_mature, 
         assign_random_tick = assign_random_tick, 
         random_tick_count = function() this_rt_count, 
         
         just_grew = just_grew, 
         set_just_grew = set_just_grew),
    class = "minecraft_block")
}

#' @rdname new_block
#' @export

print.minecraft_block <- function(x, ...){
  cat(sep = "\n", 
      "Minecraft Block", 
      sprintf("Type:                 %s", x$get_type()), 
      sprintf("P(Growth | RTick)     %s", x$grow_prob()), 
      sprintf("Is Farm:              %s", x$is_farm()), 
      sprintf("Current Growth Stage: %s", x$growth_stage()), 
      sprintf("Max Growth Stage:     %s", x$max_growth_stage()), 
      sprintf("Is Mature:            %s", x$is_mature()))
}
