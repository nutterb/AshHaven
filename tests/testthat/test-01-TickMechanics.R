# Tick Mechanices ---------------------------------------------------

test_that(
  "Probability of a Random Tick", 
  {
    expect_equal(p_rtick(), 
                 3 / 16^3)
    
    expect_equal(p_rtick(4), 
                 4 / 16^3)
    
    expect_equal(p_rtick(10), 
                 10 / 16^3)
  }
)


test_that(
  "ttrt returns expected values", 
  {
    expect_equal(ttrt(method = "median"), 
                 -1 / (log2(1 - p_rtick())))
    expect_equal(ttrt(4, method = "median"), 
                 -1 / (log2(1 - p_rtick(4))))
    
    expect_equal(ttrt(method = "mean"), 
                 1 / p_rtick())
    expect_equal(ttrt(4, method = "mean"), 
                 1 / p_rtick(4))
    
    expect_equal(ttrt(p = .25, method = "quantile"), 
                 geom_quantile(.25, growth_probability = p_rtick()))
    expect_equal(ttrt(4, p = .25, method = "quantile"), 
                 geom_quantile(.25, growth_probability = p_rtick(4)))
  }
)
