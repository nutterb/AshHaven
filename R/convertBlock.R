#' @name convertBlock
#' @title Functions to Convert Blocks to Specific Farm Types
#' 
#' @description The functions provided here operate on a `minecraft_block`
#'   object to create blocks of farmland.
#'   
#' @param block `minecraft_block` object.
#'   
#' @export

make_block_bamboo <- function(block){
  checkmate::assertClass(x = block, 
                         classes = "minecraft_block")
  
  block$set_type("Bamboo")
  block$set_grow_prob(0.15)
  block$set_farm(TRUE)
  block$set_growth_stage(0)
  block$set_max_growth_stage(15)
}

