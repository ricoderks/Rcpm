# define an input matrix according to the help documentation for the tests!
set.seed(123)

my_data <- data.frame(rt = rep(c(seq(7, 10, length.out = 5),
                                 seq(3, 6, length.out = 3), 
                                 seq(2, 5, length.out = 7),
                                 seq(2, 8, length.out = 6),
                                 seq(5, 9, length.out = 4),
                                 seq(3, 10, length.out = 6)), 3),
                      mz = rep(seq(507, 510, by = 0.1), 3),
                      carbon = c(rep("43", 5), 
                                 rep("41", 3), 
                                 rep("44", 7), 
                                 rep("46", 6), 
                                 rep("42", 4), 
                                 rep("45", 6)),
                      carbon_db = c(paste0(rep("43", 5), ":", 5:1),
                                    paste0(rep("41", 3), ":", 3:1),
                                    paste0(rep("44", 7), ":", 7:1),
                                    paste0(rep("46", 6), ":", 6:1),
                                    paste0(rep("42", 4), ":", 4:1),
                                    paste0(rep("45", 6), ":", 6:1)),
                      DotProduct = rnorm(n = 93, mean = 700, sd = 200),
                      LipidClass = c(rep("a", 31), rep("b", 31), rep("c", 31)))

my_data_x <- data.frame(mz = rep(seq(507, 510, by = 0.1), 3),
                        carbon = c(rep("43", 5), 
                                   rep("41", 3), 
                                   rep("44", 7), 
                                   rep("46", 6), 
                                   rep("42", 4), 
                                   rep("45", 6)),
                        carbon_db = c(paste0(rep("43", 5), ":", 5:1),
                                      paste0(rep("41", 3), ":", 3:1),
                                      paste0(rep("44", 7), ":", 7:1),
                                      paste0(rep("46", 6), ":", 6:1),
                                      paste0(rep("42", 4), ":", 4:1),
                                      paste0(rep("45", 6), ":", 6:1)),
                        DotProduct = rnorm(n = 93, mean = 700, sd = 200),
                        LipidClass = c(rep("a", 31), rep("b", 31), rep("c", 31)))

my_data_y <- data.frame(rt = rep(c(seq(7, 10, length.out = 5),
                                   seq(3, 6, length.out = 3), 
                                   seq(2, 5, length.out = 7),
                                   seq(2, 8, length.out = 6),
                                   seq(5, 9, length.out = 4),
                                   seq(3, 10, length.out = 6)), 3),
                        carbon = c(rep("43", 5), 
                                   rep("41", 3), 
                                   rep("44", 7), 
                                   rep("46", 6), 
                                   rep("42", 4), 
                                   rep("45", 6)),
                        carbon_db = c(paste0(rep("43", 5), ":", 5:1),
                                      paste0(rep("41", 3), ":", 3:1),
                                      paste0(rep("44", 7), ":", 7:1),
                                      paste0(rep("46", 6), ":", 6:1),
                                      paste0(rep("42", 4), ":", 4:1),
                                      paste0(rep("45", 6), ":", 6:1)),
                        DotProduct = rnorm(n = 93, mean = 700, sd = 200),
                        LipidClass = c(rep("a", 31), rep("b", 31), rep("c", 31)))

my_data_colorby <- data.frame(rt = rep(c(seq(7, 10, length.out = 5),
                                          seq(3, 6, length.out = 3), 
                                          seq(2, 5, length.out = 7),
                                          seq(2, 8, length.out = 6),
                                          seq(5, 9, length.out = 4),
                                          seq(3, 10, length.out = 6)), 3),
                               mz = rep(seq(507, 510, by = 0.1), 3),
                               carbon_db = c(paste0(rep("43", 5), ":", 5:1),
                                             paste0(rep("41", 3), ":", 3:1),
                                             paste0(rep("44", 7), ":", 7:1),
                                             paste0(rep("46", 6), ":", 6:1),
                                             paste0(rep("42", 4), ":", 4:1),
                                             paste0(rep("45", 6), ":", 6:1)),
                               DotProduct = rnorm(n = 93, mean = 700, sd = 200),
                               LipidClass = c(rep("a", 31), rep("b", 31), rep("c", 31)))

my_data_carbondb<- data.frame(rt = rep(c(seq(7, 10, length.out = 5),
                                          seq(3, 6, length.out = 3), 
                                          seq(2, 5, length.out = 7),
                                          seq(2, 8, length.out = 6),
                                          seq(5, 9, length.out = 4),
                                          seq(3, 10, length.out = 6)), 3),
                               mz = rep(seq(507, 510, by = 0.1), 3),
                               carbon = c(rep("43", 5), 
                                          rep("41", 3), 
                                          rep("44", 7), 
                                          rep("46", 6), 
                                          rep("42", 4), 
                                          rep("45", 6)),
                               DotProduct = rnorm(n = 93, mean = 700, sd = 200),
                               LipidClass = c(rep("a", 31), rep("b", 31), rep("c", 31)))

my_matrix <- matrix(data = 1, ncol = 3, nrow = 5)

my_bubbleplot <- Rcpm::bubble_plot(data = my_data, 
                             x = rt, 
                             y = mz, 
                             color_by = carbon, 
                             carbon_db = carbon_db)

context("bubble-plot: input check")

# from the function bubble_plot I expect the following:
# input:
# * not enough arguments
# * if input is not a data frame, show an error message
# * if the give column name is not found, show an error message

test_that("Is input correct data frame:", {
  expect_error(bubble_plot(data = my_data), "Not enough arguments passed... ")
  expect_error(bubble_plot(data = my_matrix, x = rt, y = mz, color_by = carbon, carbon_db = carbon_db), 
               "'data' does not appear to be a data frame!")
  expect_error(bubble_plot(data = my_data_x, x = rt, y = mz, color_by = carbon, carbon_db = carbon_db), 
               "'rt' is not the name of a variable in 'my_data_x'")
  expect_error(bubble_plot(data = my_data_y, x = rt, y = mz, color_by = carbon, carbon_db = carbon_db), 
               "'mz' is not the name of a variable in 'my_data_y'")
  expect_error(bubble_plot(data = my_data_colorby, x = rt, y = mz, color_by = carbon, carbon_db = carbon_db), 
               "'carbon' is not the name of a variable in 'my_data_colorby'")
  expect_error(bubble_plot(data = my_data_carbondb, x = rt, y = mz, color_by = carbon, carbon_db = carbon_db), 
               "'carbon_db' is not the name of a variable in 'my_data_carbondb'")
})

context("bubble-plot: output check")

# from the function bubble_plot I expect the following:
# Output:
# * an ggplot object (class is gg and ggplot)
# * compare the plot

test_that("Is output correct ggplot2 object:", {
  expect_equal(class(bubble_plot(data = my_data, x = rt, y = mz, color_by = carbon, carbon_db = carbon_db)), 
               c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"))
  vdiffr::expect_doppelganger("ggplot2 graph", my_bubbleplot)
})
