context("Promises")

`%is%` <- expect_equal

test_that("can create quotation explicitly", {
  x <- 5
  y <- quo(x+1)
  delayedAssign()
  expect_identical(y, quo(x+1))
  expect_false(identical(y, quo(x+2)))
  x <- 6
  y() %is% 7
  x <- 7
  y() %is% 8
})

test_that("can force quotation, and make forced quotations, and forced", {
  x <- 1
  q <- quo(x <- x + 1)
  forced(q) %is% FALSE
  value(q) %is% 2
  value(q) %is% 3 #re-runs
  x %is% 3
  fq <- quo(x <- x + 1, force=TRUE)
  forced(q) %is% TRUE
  x %is% 4
  expr(fq) %is% quo(x <- x + 1)
  env(fq) %is% R_EmptyEnv()
  value(fq) %is% 4 #forced at quo creation
  value(fq) %is% 4 #not re-forced
})

test_that("can get expr and environment of quo", {
  where <- "top"
  f <- function(x = x+y) {
    where <- "f"
    arg(x)
  }
  q1 <- f()
  expr(q1) %is% quote(x+y)
  env(q1)$where %is% "f"

  q2 <- f(z+zzz)
  expr(q2) %is% quote(z+zzz)
  env(q1)$where %is% "top"
})

test_that("Can get missingness and forcedness of quo", {
  w <- 1
  x <- missing_value()
  delayedAssign("y", quote(x))
  delayedAssign("z", quote(stop("Should not force")))
  missing_(arg(w)) %is% FALSE
  missing_(arg(x)) %is% TRUE
  missing_(arg(y)) %is% TRUE
  missing_(arg(z)) %is% FALSE
  forced_(arg(w)) %is% TRUE
  forced_(arg(x)) %is% TRUE
  forced_(arg(y)) %is% FALSE
  forced_(arg(z)) %is% FALSE
})
