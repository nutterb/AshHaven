# Perpetual Furnace Index -------------------------------------------

test_that(
  "Perpetual Furnace Calculations", 
  {
    # Raw Bamboo
    expect_equal(round(perpetual_furnace_index(total_smelt = 0.25, 
                                               self_smelt = 0, 
                                               craft_consumption = 1, 
                                               yield = 1, 
                                               growth_probability = 1/3, 
                                               stages = 1), 3), 
                 32.345)
    
    # Bamboo Planks
    expect_equal(round(perpetual_furnace_index(total_smelt = 3, 
                                               self_smelt = 0, 
                                               craft_consumption = 9, 
                                               yield = 1, 
                                               growth_probability = 1/3, 
                                               stages = 1), 3), 
                 24.259)
    
    # Kelp 
    expect_equal(round(perpetual_furnace_index(total_smelt = 20, 
                                               self_smelt = 9, 
                                               craft_consumption = 9, 
                                               yield = 1, 
                                               growth_probability = 0.14, 
                                               stages = 1), 3), 
                 17.786)
  }
)
