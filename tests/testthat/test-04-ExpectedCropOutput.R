# Expected Crop Output ----------------------------------------------

test_that(
  "Expected Crop Output of Bamboo", 
  {
    expect_equal(round(expected_crop_output(1, 1/3, 1), 3), 
                 0.742)
    expect_equal(round(expected_crop_output(1, 1/3, 1, method = "mean"), 3), 
                 0.293)
  }
)

test_that(
  "Expected Crop Output of Wheet Seeds", 
  {
    expect_equal(round(expected_crop_output(2 + 5/7, 1/3, 8), 3), 
                 0.252)
    expect_equal(round(expected_crop_output(2 + 5/7, 1/3, 8, method = "mean"), 3), 
                 0.099)
  }
)
