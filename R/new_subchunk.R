new_subchunk <- function(x = 16, 
                         y = 16, 
                         z = 16){
  block_list <- replicate(x * y * z, 
                          new_block(), 
                          simplify = FALSE)
  
  subchunk_apply <- function(index, fun){
    invisible(lapply(block_list[index], fun))
  }
  
  assign_random_ticks <- function(rts = getOption("randomTickSpeed", 3)){
    idx <- sample(seq_along(block_list), 
                  size = rts, 
                  replace = TRUE)
    
    for (i in idx){
      block_list[[i]]$assign_random_tick()
    }
  }
  
  compile_data <- function(){
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
                 compile_data = compile_data),
            class = "minecraft_subchunk")
}




S <- new_subchunk()

S$compile_data()
