# Time to Maturity --------------------------------------------------

test_that(
  "Time to Maturity for Bamboo", 
  {
    expect_equal(round(time_to_maturity(1/3, 1), 7), 
                 1.3477079)
    expect_equal(round(time_to_maturity(1/3, 1, method = "mean"), 7), 
                 3.4133333)
    expect_equal(round(time_to_maturity(1/3, 1, p = .9, method = "quantile"), 7), 
                 13.0916667)
    
    expect_equal(round(time_to_maturity(1/3, 11), 7), 
                 14.8247874)
    expect_equal(round(time_to_maturity(1/3, 11, method = "mean"), 7), 
                 37.5466667)
    expect_equal(round(time_to_maturity(1/3, 11, p = .9, method = "quantile"), 7), 
                 144.0083333)
  }
)

test_that(
  "Time to Maturity for Wheat", 
  {
    expect_equal(round(time_to_maturity(1/3, 8), 7), 
                 10.7816636)
    expect_equal(round(time_to_maturity(1/3, 8, method = "mean"), 7), 
                 27.3066667)
    expect_equal(round(time_to_maturity(1/3, 8, p = .9, method = "quantile"), 7), 
                 104.7333333)
  }
)
