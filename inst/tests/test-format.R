context("formatting")

`%is%` <- function(...) expect_equal(..., expected.label = ..2)

test_that("dots_unpack has a print method that works", {
  x <- capture.output(as.data.frame(dots(a, b, c, d, 4, e)))
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

test_that("format dots and quotations", {
  x <- 2
  force_first_and_a <- function(..., a) list(a, ..1)

  dots_some_forced <- function(...) {
    force_first_and_a(...)
    dots(...)
  }

  e <- format(environment())

  d <- dots_some_forced(4, a=x+2, b+1, c=3+3, "5")

  format(d) %is%
    paste0("dots<< 4, a = x + 2 := 4, ", e, " ? b + 1, c = ", e, " ? 3 + 3, ", e, " ? \"5\" >>")
  format(d, show.environments=FALSE) %is%
    paste0("dots<< 4, a = x + 2 := 4, ? b + 1, c = ? 3 + 3, ? \"5\" >>")
  format(d, show.expressions=FALSE) %is%
    paste0("dots<< 4, a = 4, ", e, " ? b + 1, c = ", e, " ? 3 + 3, ", e, " ? \"5\" >>")
  format(d, compact=TRUE) %is%
    paste0("dots<< 4, a = 4, ? b + 1, c = ? 3 + 3, ? \"5\" >>")

  format(d[[1]]) %is% "quo<< 4 >>"
  format(d[[2]]) %is% "quo<< x + 2 := 4 >>"
  format(d[[3]]) %is% paste0("quo<< ", e, " ? b + 1 >>")
  format(d[[4]]) %is% paste0("quo<< ", e, " ? 3 + 3 >>")
  format(d[[5]]) %is% paste0("quo<< ", e, " ? \"5\" >>")

  format(dots(a, b, c)) %is% paste0("dots<< ", e, " ? a, ", e, " ? b, ", e, " ? c >>")
})

test_that("format outputs one line", {
  expect_equal(length(format(dots(a = function(x){x}))), 1)
  expect_equal(length(as.dots.literal(list(a = function(x){x}))), 1)
})
