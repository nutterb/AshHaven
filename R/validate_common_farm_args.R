#' @name validate_common_farm_args
#' @title Validate Common Farm Arguments
#' 
#' @description A utility function to validate the arguments that are commonly 
#'   passed with farm calculations, such as farm size, random tick speed, 
#'   or subchunk size. 
#'   
#' @param f `integerish(1)`. The number of blocks in the chunk that have 
#'   been set aside as farm land.
#' @param fr `integerish`.  The number of blocks of farmland receiving 
#'   a random tick. Greater than 0, but less than `n`. If passed as 
#'  `NULL`, it will not be validated.
#' @param random_tick_speed `integerish(1)`. The number of blocks in the chunk to receive a 
#'   random tick. Defaults to the random tick speed. Must be less than `N`.
#' @param B `integerish(1)`. The number of blocks in a chunk. Defaults to `16^3`.
#' @param coll An `assertCollection` object.
#' 
#' @export

validate_common_farm_args <- function(f, 
                                      random_tick_speed, 
                                      B, 
                                      fr = NULL, 
                                      coll){
  checkmate::assertIntegerish(x = f, 
                              len = 1,
                              lower = 0, 
                              upper = B, 
                              add = coll)
  
  checkmate::assertIntegerish(x = random_tick_speed, 
                              len = 1, 
                              lower = 0, 
                              upper = B, 
                              add = coll)
  
  checkmate::assertIntegerish(x = B, 
                              len = 1, 
                              add = coll)
  
  if (!is.null(fr)){
    checkmate::assertIntegerish(x = fr, 
                                lower = 0, 
                                upper = random_tick_speed, 
                                add = coll)
  }
}