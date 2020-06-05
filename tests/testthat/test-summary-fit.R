# define an input matrix according to the help documentation for the tests!
sumfit <- data.frame(PC = paste0("PC", 1:5),
                     R2cum = seq(from = 0.1, to = 0.75, length.out = 5),
                     Q2cum = seq(from = 0.05, to = 0.5, length.out = 5))

my_matrix <- matrix(data = 1, ncol = 3, nrow = 5)

sumfit_wrong <- data.frame(PC = paste0("PC", 1:5),
                           R2cu = seq(from = 0.1, to = 0.75, length.out = 5),
                           Q2cum = seq(from = 0.05, to = 0.5, length.out = 5))


context("summary-fit: input check")

# from the function summary_fit_data I expect the following:
# input:
# * if input is not a data frame an error message
# * if input data frame doesn't have the correct column names an error

test_that("Is input correct data frame", {
  expect_error(summary_fit(my_matrix), "data needs to be a data frame!")
  expect_error(summary_fit(sumfit_wrong), "data should contain only the column names PC, R2cum and Q2cum!")
})


context("summary-fit: output check")

# from the function summary_fit_data I expect the following:
# Output:
# * an ggplot object (class is gg and ggplot)

test_that("Is output correct data frame", {
  expect_equal(class(summary_fit(sumfit)), c("gg", "ggplot"))
})