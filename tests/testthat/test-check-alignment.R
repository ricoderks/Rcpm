context("check-alignment: input check")

test_that("Input check:", {
  expect_error(check_alignment(files = "./file.mzxml",
                               "File doesn't exist!"))
})

test_that("Output check:", {
  expect_equal(check_alignment(files = "./myfile.mzXML"),
               TRUE)
})