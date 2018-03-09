context("formatting")

`%is%` <- function(...) expect_equal(..., expected.label = ..2)

test_that("dots_unpack has a print method that works", {
  x <- capture.output(dots_unpack(a, b, c, d, 4, e))
  expect_equal(length(x), 7)
})

silently <- function(x, output_callback = force) {
  y <- NULL
  output_callback(capture.output(y <- print(x)))
  y
}

test_that("dots has some kind of print method", {
  d <- dots(a, b, c)
  expect_true(identical(silently(print(d)), d))
})

test_that("format dots", {
  x <- 2
  force_first_and_a <- function(..., a) list(a, ..1)
  dots_forced <- function(...) {
    force_first_and_a(...)
    dots(...)
  }

  e <- format(environment())

  d <- dots_forced(4, a=x+2, b+1, c=3+3)

  format(d) %is% paste0("args(4, a = x + 2 := 4, ", e, " ? b + 1, c = ", e, " ? 3 + 3)")
  format(d, show.environments=FALSE) %is% paste0("args(4, a = x + 2 := 4, ? b + 1, c = ? 3 + 3)")
  format(d, show.expressions=FALSE) %is% paste0("args(4, a = 4, ", e, " ? b + 1, c = ", e, " ? 3 + 3)")
  format(d, compact=TRUE) %is% paste0("args(4, a = 4, ? b + 1, c = ? 3 + 3)")

  format(dots(a, b, c)) %is% paste0("args(", e, " ? a, ", e, " ? b, ", e, " ? c)")
})

test_that("format outputs one line", {
  expect_equal(length(format(dots(a = function(x){x}))), 1)
  expect_equal(length(as.dots.literal(list(a = function(x){x}))), 1)
})
