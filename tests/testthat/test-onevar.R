test_that("Object returned is of class ggplot", {
  df <- data.frame(x = c("a", "b", "b", "c", "c", "c"))
  p <- onevar(df, x)
  expect_equal(class(p)[[2]], "ggplot")
})
