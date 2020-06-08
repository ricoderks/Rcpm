# define an input matrix according to the help documentation for the tests!

# test the input
context("ms-search-lipidmaps: input check")

test_that("Is input correct:", {
  expect_error(ms_search_lipidmaps(), 
               "'ExactMass' not set!")
  expect_error(ms_search_lipidmaps(ExactMass = -537.37944), 
               "'ExactMass' should be possitive!")
  expect_error(ms_search_lipidmaps(ExactMass = "aap"), 
               "'ExactMass' needs to be a number!")
  expect_error(ms_search_lipidmaps(ExactMass = 537.37944, 
                                   ExactMassOffSet = -0.01), 
               "'ExactMassOffSet' should be possitive!")
  expect_error(ms_search_lipidmaps(ExactMass = 537.37944, 
                                   ExactMassOffSet = "aap"), 
               "'ExactMassOffSet' needs to be a number!")
  expect_error(ms_search_lipidmaps(ExactMass = 537.37944, 
                                   ExactMassOffSet = 0.01,
                                   CoreClass = 9), 
               "'CoreClass' needs to be between 1 and 8!")
  expect_error(ms_search_lipidmaps(ExactMass = 537.37944, 
                                   ExactMassOffSet = 0.01,
                                   CoreClass = "9"), 
               "'CoreClass' needs to be a number!")
})

# test the output
context("ms-search-lipidmaps: output check")

test_that("Is the output correct:", {
  expect_equal(class(ms_search_lipidmaps(ExactMass = 537.37944)), 
               "data.frame")
})