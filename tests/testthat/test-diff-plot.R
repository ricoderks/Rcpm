# define an input matrix according to the help documentation for the tests!
set.seed(123)

my_data <- data.frame(lipidname = c(paste("PA ", seq(30, 40, 2), ":0", sep = ""),
                                    paste("PA ", seq(30, 40, 2), ":1", sep = ""),
                                    paste("PA ", seq(30, 40, 2), ":2", sep = ""),
                                    paste("PC ", seq(30, 40, 2), ":0", sep = ""),
                                    paste("PC ", seq(30, 40, 2), ":1", sep = ""),
                                    paste("PC ", seq(30, 40, 2), ":2", sep = ""),
                                    paste("TG ", seq(50, 60, 1), ":0", sep = ""),
                                    paste("TG ", seq(50, 60, 1), ":1", sep = ""),
                                    paste("TG ", seq(50, 60, 1), ":2", sep = ""),
                                    paste("TG ", seq(50, 60, 1), ":3", sep = ""),
                                    paste("TG ", seq(50, 60, 1), ":4", sep = "")),
                      lipidclass = c(rep("PA", 18),
                                     rep("PC", 18),
                                     rep("TG", 55)),
                      difference = rnorm(n = 91, 
                                         mean = 0,
                                         sd = 3e4),
                      versus = factor(x = "AvsB"))

my_data$diff_grp <- as.factor(ifelse(my_data$difference > 0, "high", "low"))

my_diffplot <- diff_plot(data = my_data,
                         x = lipidname,
                         y = difference,
                         fill_by = diff_grp,
                         facet_x = versus,
                         facet_y = lipidclass)

my_matrix <- matrix(data = 1, ncol = 3, nrow = 5)

my_data_x <- my_data[, -1]
my_data_y <- my_data[, -3]
my_data_fillby <- my_data[, -5]
my_data_facetx <- my_data[, -4]
my_data_facety <- my_data[, -2]

context("diff-plot: input check")

test_that("Is input correct data frame:", {
  expect_error(diff_plot(data = my_data,
                         x = lipidname,
                         y = difference,
                         facet_x = versus,
                         facet_y = lipidclass), 
               "Not enough arguments passed... ")
  expect_error(diff_plot(data = my_matrix,
                         x = lipidname,
                         y = difference,
                         fill_by = diff_grp,
                         facet_x = versus,
                         facet_y = lipidclass), 
               "'data' does not appear to be a data frame!")
  expect_error(diff_plot(data = my_data_x,
                         x = lipidname,
                         y = difference,
                         fill_by = diff_grp,
                         facet_x = versus,
                         facet_y = lipidclass), 
               "'lipidname' is not the name of a variable in 'my_data_x'")
  expect_error(diff_plot(data = my_data_y,
                         x = lipidname,
                         y = difference,
                         fill_by = diff_grp,
                         facet_x = versus,
                         facet_y = lipidclass), 
               "'difference' is not the name of a variable in 'my_data_y'")
  expect_error(diff_plot(data = my_data_fillby,
                         x = lipidname,
                         y = difference,
                         fill_by = diff_grp,
                         facet_x = versus,
                         facet_y = lipidclass), 
               "'diff_grp' is not the name of a variable in 'my_data_fillby'")
  expect_error(diff_plot(data = my_data_facetx,
                         x = lipidname,
                         y = difference,
                         fill_by = diff_grp,
                         facet_x = versus,
                         facet_y = lipidclass), 
               "'versus' is not the name of a variable in 'my_data_facetx'")
  expect_error(diff_plot(data = my_data_facety,
                         x = lipidname,
                         y = difference,
                         fill_by = diff_grp,
                         facet_x = versus,
                         facet_y = lipidclass), 
               "'lipidclass' is not the name of a variable in 'my_data_facety'")
})

context("bubble-plot: output check")

test_that("Is output correct ggplot2 object:", {
  expect_equal(class(diff_plot(data = my_data,
                               x = lipidname,
                               y = difference,
                               fill_by = diff_grp,
                               facet_x = versus,
                               facet_y = lipidclass)),
               c("gg", "ggplot"))
  vdiffr::expect_doppelganger("My diff plot", my_diffplot)
})
