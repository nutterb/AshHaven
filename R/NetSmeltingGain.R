#' @name NetSmeltingGain
#' @title Net Smelting Gain
#' 
#' @description The Net Smelting Gain (NSG) is a measure of the number of 
#'  items an individual fuel source item can smelt after accounting for the 
#'  number of its own items it must consume in order to turn itself into a fuel 
#'  source.
#'  
#' @param total_smelt `numeric(1)`. The total number of items an item can 
#'   smelt. May be a fractional part.
#' @param self_smelt `numeric(1)`. The number of items of the same type that
#'   must be spent turn the item into fuel.
#' @param craft_consumption `numeric(1)`. The number of the items used in 
#'   crafting a more complex item for fuel.
#'   
#' @description As an example, kelp may be used as a fuel source after it has
#'   been smelted and crafted into a dried kelp block. A dried kelp block
#'   is able to smelt twenty items (`total_smelt = 20`), requires 9 items to 
#'   be smelted to become fuel (`self_smelt = 9`), and 9 items are consumed 
#'   in the crafting of a dried kelp block (`craft_consumption = 9`). This 
#'   results in a net smelting gain of 1.22 items per kelp.
#'   
#'   Bamboo is able to smelt 0.25 items (`total_smelt = 0.25`), requires zero
#'   items be smelted to become fuel (`self_smelt = 0`), and one item is 
#'   consumed in making bamboo into fuel (itself, `craft_consumption = 1`). 
#'   This results in a net smelting gain of 0.25 items per bamboo.
#'   
#' @export

net_smelting_gain <- function(total_smelt, 
                              self_smelt, 
                              craft_consumption = 1){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_numeric(x = total_smelt, 
                            len = 1, 
                            lower = 0,
                            add = coll)
  
  checkmate::assert_numeric(x = self_smelt, 
                            len = 1, 
                            lower = 0, 
                            add = coll)
  
  checkmate::assert_numeric(x = craft_consumption, 
                            len = 1, 
                            lower = 0, 
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  (total_smelt - self_smelt) / craft_consumption
}
