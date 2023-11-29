# Net Smelting Gain -------------------------------------------------

test_that(
  "Net Smelting Gain", 
  {
    # Raw Bamboo
    expect_equal(round(net_smelting_gain(0.25, 0, 1), 3), 
                 0.25)
    
    # Bamboo Planks
    expect_equal(round(net_smelting_gain(3, 0, 9), 3), 
                 0.333)
    
    # Dried Kelp Block
    expect_equal(net_smelting_gain(20, 9, 9), 
                 11/9)
  }
)
