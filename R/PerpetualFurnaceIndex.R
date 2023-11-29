#' @name PerpetualFurnaceIndex
#' @title Perpetual Furnace Index
#' 
#' @description The Perpetual Furnace Index (PFI) is the number blocks of 
#'   farm that are required to maintain continuous and uninterupted operation 
#'   of a single furnace in a self sufficient manner. Furnaces smelt six items 
#'   per minute, so this is the number of blocks of farm required to provide 
#'   enough fuel to smelt six items per minute. The PFI must also account for 
#'   raw materials required to convert itself into fuel 
#'   (see [net_smelting_gain()]).
#'   
#' @inheritParams NetSmeltingGain
#' @inheritParams TimeToGrowth
#' @inheritParams TimeToMaturity
#' 
#' @export 

perpetual_furnace_index <- function(total_smelt, 
                                    self_smelt, 
                                    craft_consumption,
                                    yield, 
                                    growth_probability, 
                                    stages,
                                    p, 
                                    method = c("median", "mean", "quantile")){
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
  
  checkmate::assert_numeric(x = yield, 
                            len = 1,
                            lower = 0,
                            add = coll)
  
  checkmate::assert_numeric(x = growth_probability, 
                            len = 1, 
                            lower = 0, 
                            upper = 1, 
                            add = coll)
  
  checkmate::assert_integerish(x = stages, 
                               len = 1, 
                               lower = 1, 
                               add = coll)
  
  checkmate::assert_numeric(x = p, 
                            len = 1, 
                            lower = 0, 
                            upper = 1, 
                            add = coll)
  
  method <- checkmate::matchArg(x = method,
                                choices = c("median", "mean", "quantile"), 
                                .var.name = "method",
                                add = coll)
  
  checkmate::reportAssertions(coll)
  
  eco <- expected_crop_output(yield = yield, 
                              growth_probability = growth_probability,
                              stages = stages,
                              p = p, 
                              method = method)
  nsg <- net_smelting_gain(total_smelt = total_smelt, 
                           self_smelt = self_smelt, 
                           craft_consumption = craft_consumption)
  
  (6 * craft_consumption) / (eco * nsg)
}
