context("Promises")

`%is%` <- expect_equal

test_that("can create quotation explicitly", {
  x <- 5
  y <- quo(x+1)
  expect_identical(y, quo(x+1))
  expect_false(identical(y, quo(x+2)))
  x <- 6
  value(y) %is% 7
  x <- 7
  value(y) %is% 8
})

test_that("can force quotation, and make forced quotations, and forced", {
  x <- 1
  q <- quo(x <- x + 1)
  forced(q) %is% FALSE
  value(q) %is% 2
  value(q) %is% 3 #re-runs
  x %is% 3
  fq <- quo(x <- x + 1, force = TRUE)
  forced(fq) %is% TRUE
  x %is% 4
  expr(fq) %is% quote(x <- x + 1)
  env(fq) %is% emptyenv()
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
  env(q2)$where %is% "top"
})

test_that("Can get missingness and forcedness of quo", {
  w <- 1
  x <- missing_value()
  delayedAssign("y", quote(x))
  delayedAssign("z", quote(stop("Should not force")))
  missing_(arg(w)) %is% FALSE
  missing_(arg(x)) %is% TRUE
  missing_(arg(y)) %is% FALSE
  missing_(arg(z)) %is% FALSE
  forced(arg(w)) %is% TRUE
  forced(arg(x)) %is% FALSE
  forced(arg(y)) %is% FALSE
  forced(arg(z)) %is% FALSE
})

test_that("Can force quo with alternate eval", {
  # Note, the only good reason to do this is if you're implementing eval.
  x <- quo(asdlaksj + qweoiu)
  x <- force_(x, eval= function(expr, envir) 5)
  expect_true(forced(x))
  value(x) %is% 5
})

test_that("forced quos", {
  forced(forced_quo(6)) %is% TRUE
  expr(forced_quo(6)) %is% 6
  env(forced_quo(6)) %is% emptyenv()

  value(forced_quo(2+5)) %is% 7
  forced(forced_quo(2+5)) %is% TRUE
  env(forced_quo(2+5)) %is% emptyenv()
  expr(forced_quo(2+5)) %is% quote(2+5)
  expr(forced_quo_(2+5)) %is% 7
  env(forced_quo_(as.name("x"))) %is% emptyenv()
  expr(forced_quo_(as.name("x"))) %is% quote(quote(x))
  value(forced_quo_(as.name("x"))) %is% as.name("x")
  x <- 5
  expr(forced_quo(x)) %is% quote(x)
  value(forced_quo(x)) %is% 5
  value(forced_quo(as.name("x"))) %is% as.name("x")

  x <- 3
  do(list, forced_quo_(quote(x))) %is% alist(x)
  do(list, forced_quo_(quote(x+y))) %is% alist(x+y)
})
