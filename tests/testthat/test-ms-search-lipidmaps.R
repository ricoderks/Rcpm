
context("ms-search-lipidmaps: input check")


test_that("Is input correct", {
  expect_error(ms_search_lipidmaps(), "'ExactMass' not set!")
  expect_error(ms_search_lipidmaps(ExactMass = -205.0), "'ExactMass' should be possitive!")
  expect_error(ms_search_lipidmaps(ExactMass = "aap"), "'ExactMass' needs to be a number!")
  expect_error(ms_search_lipidmaps(ExactMass = 205.0, 
                                   ExactMassOffSet = -0.01), "'ExactMassOffSet' should be possitive!")
  expect_error(ms_search_lipidmaps(ExactMass = 205.0, 
                                   ExactMassOffSet = "aap"), "'ExactMassOffSet' needs to be a number!")
})


