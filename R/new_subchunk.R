#' @name new_subchunk
#' @title Create Subchunk Object for Simulation
#' 
#' @description Creates an object with utilities for
#'  monitoring farm productivity in simulation.
#'  
#' @param x,y,z `integerish(1)` the number of blocks that
#'   make up the x, y, and z dimensions of the subchunk.
#'   Each defaults to 16.
#'   
#' @export

new_subchunk <- function(x = 16, 
                         y = 16, 
                         z = 16){
  
  block_list <- replicate(x * y * z, 
                          new_block(), 
                          simplify = FALSE)
  tick_counter <- 0
  this_tick_history <- list()
  
  # Farm Evaluations ------------------------------------------------
  
  all_mature <- function(){
    fl <- which(vapply(seq_along(block_list), 
                       function(i) block_list[[i]]$is_farm(), 
                       logical(1)))
    all(vapply(fl, 
               function(i) block_list[[i]]$is_mature(), 
               logical(1)))
  }
  
  subchunk_apply <- function(index, fun, ...){
    invisible(lapply(block_list[index], fun, ...))
  }
  
  assign_random_ticks <- function(rts = getOption("randomTickSpeed", 3)){
    tick_counter <<- tick_counter + 1

    idx <- sample(seq_along(block_list),
                  size = rts,
                  replace = TRUE)

    invisible(lapply(idx, function(i) block_list[[i]]$assign_random_tick()))

    is_farm <- vapply(idx,
                      function(i) block_list[[i]]$is_farm(),
                      logical(1))

    just_grew <- vapply(idx,
                        function(i) block_list[[i]]$just_grew(),
                        logical(1))
  
    is_mature <- vapply(idx[is_farm], 
                        function(i) block_list[[i]]$is_mature(), 
                        logical(1))

    this_tick_history[[tick_counter]] <<- 
      data.frame(tick_number = tick_counter,
                 rticked_farm = sum(is_farm),
                 rticked_not_farm = sum(!is_farm),
                 n_grew = sum(just_grew), 
                 n_mature = sum(is_mature))

    invisible(lapply(idx, function(i) block_list[[i]]$set_just_grew()))
  }
  
  block_status <- function(){
    Blocks <- block_list
    
    data.frame(
      type = vapply(Blocks,
                    function(b) b$get_type(),
                    character(1)),
      is_farm = vapply(Blocks,
                       function(b) b$is_farm(),
                       logical(1)),
      grow_prob = vapply(Blocks,
                         function(b) b$grow_prob(),
                         numeric(1)),
      growth_stage = vapply(Blocks,
                            function(b) b$growth_stage(),
                            numeric(1)),
      is_mature = vapply(Blocks,
                         function(b) b$is_mature(),
                         logical(1)),
      random_tick_count = vapply(Blocks,
                                 function(b) b$random_tick_count(),
                                 numeric(1)),
      stringsAsFactors = FALSE
    )
  }
  
  
  structure(list(blocks = function() block_list, 
                 subchunk_apply = subchunk_apply, 
                 assign_random_ticks = assign_random_ticks, 
                 block_status = block_status, 
                 tick_history = function() do.call("rbind", this_tick_history),
                 all_mature = all_mature),
            class = "minecraft_subchunk")
}
