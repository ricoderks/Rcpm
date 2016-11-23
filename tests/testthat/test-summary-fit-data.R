context("summary-fit-data")

# from the function summary_fit_data I expect the following:
# Output:
# * a data frame
# * column names are PC, variable, value

# define an input matrix according to the help documentation
sumfit <- data.frame(PC = paste0("PC", 1:5),
                     R2cum = seq(from = 0.1, to = 0.75, length.out = 5),
                     Q2cum = seq(from = 0.05, to = 0.5, length.out = 5))

test_that("Is output correct data frame", {
  expect_equal(class(summary_fit_data(sumfit)), "data.frame")
  expect_equal(colnames(summary_fit_data(sumfit)), c("PC", "variable", "value"))
  expect_equal(dim(summary_fit_data(sumfit)), c(10, 3))
})