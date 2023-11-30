# Time to Growth ----------------------------------------------------

test_that(
  "Time to Growth for Bamboo", 
  {
    expect_equal(round(time_to_growth(1/3, method = "median"), 7), 
                 1.3477079)
    expect_equal(round(time_to_growth(1/3, method = "mean"), 7), 
                 3.4133333)
    expect_equal(round(time_to_growth(1/3, p = .90, method = "quantile"), 3), 
                 14.872)
  }
)

test_that(
  "Time to Growth for Kelp", 
  {
    expect_equal(round(time_to_growth(0.14, method = "median"), 7), 
                 3.6231142)
    expect_equal(round(time_to_growth(0.14, method = "mean"), 7), 
                 8.1269841)
    expect_equal(round(time_to_growth(0.14, p = .90, method = "quantile"), 3), 
                 39.982)
  }
)
