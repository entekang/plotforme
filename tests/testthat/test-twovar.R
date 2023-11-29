test_that("Object returned is of class ggplot", {
  df <- data.frame(x = c(rep("a", 10), rep("b", 10)), y = rnorm(20))
  p <- twovar(df, x, y)                     # this will be a list of three plots
  expect_equal(class(p[[1]])[[2]], "ggplot")
  expect_equal(class(p[[2]])[[2]], "ggplot")
  expect_equal(class(p[[3]])[[2]], "ggplot")
})
