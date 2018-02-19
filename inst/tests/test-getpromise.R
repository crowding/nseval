context("Promise extraction")

`%is%` <- expect_equal

test_that("can recover environments of arguments", {

  f1 <- function(a, ...) { #a=one@top, two@top
    b <- 1
    where = "f1"
    f2(b, where, ...) #b, where, two
  }
  f2 <- function(a, ...) { # a=b@f1, where@f1, two@top
    b <- 2
    where <- "f2"
    f3(b, where, ...) #b@f2, where@f2, where@f1, two@top
  }
  f3 <- function(a, b, c, ...) { #a=b@f2, b=where@f2, c=where@f1, two@top
    arg_expr(a) %is% quote(b)
    arg_expr(b) %is% quote(where)
    arg_expr(c) %is% quote(where)
    exprs(dots(...)) %is% alist(two)
    arg_env(a)$where %is% "f2"
    arg_env(b)$where %is% "f2"
    arg_env(c)$where %is% "f1"
    envs(dots(...))[[1]]$where %is% "top"
  }
  where <- "top"
  f1(one, two)
})

test_that("arg_env error on forced promise", {
  f1 <- function(x) arg_env(x)
  f2 <- function(...) {
    list(...)
    f1(...)
  }
  expect_error(f2(12+12), "forced")
  expect_equal(f2(124), emptyenv())
})

test_that("arg_expr should not force promise", {
  e <- environment()
  f <- function(x) {
    expect_equal(arg_expr(x), quote(y+z))
    expect_equal(arg_env(x), e)
    expect_equal(arg_expr(x), quote(y+z))
    expect_equal(arg_env(x), e)
  }
  f(y+z)
})

test_that("arg_expr and arg_env fudge when could have been literal.", {
  # R will usually do a small optimization by not bothering to
  # construct promises for arguments that are a literal in the
  # source. Therefore we will have to allow these cases with arg_expr
  # and arg_env -- returning emptyenv when it is safe to do so.
  e <- environment()

  normal <- function(x) {
    list(arg_expr(x), arg_env(x))
  }
  normal(2000+3000) %is% list(quote(2000+3000), environment())
  # for extra credit explain why optimization happens here
  (function() normal(5000))() %is% list(5000, emptyenv())
  # but not always here
  # normal(5000) %is% list(5000, emptyenv())
})

test_that("arg_expr and arg_env when expression is already forced.", {
  # But when the promise is forced?
  force_then_expr <- function(x) {
    force(x)
    arg_expr(x)
  }
  force_then_env <- function(x) {
    force(x)
    arg_env(x)
  }
  force_then_expr(2000+3000) %is% quote(2000+3000)
  expect_error(force_then_env(2000+3000))
  force_then_expr(5000) %is% 5000 # not a promise
  force_then_env(5000) %is% emptyenv() #not a promise
  force_then_expr(5000L) %is% 5000L #not a promise
  force_then_env(5000L) %is% emptyenv() #not a promise
  force_then_expr(quote(x)) %is% quote(quote(x)) #language object
  expect_error(force_then_env(quote(x)))
})

test_that("arg_expr and arg_env when expression is not a promise", {
  # and what about bindings that are not promises?
     nonpromise_expr <- function(x) {
       y <- x
       arg_expr(y)
     }
     nonpromise_env <- function(x) {
       y <- x
       arg_env(y)
     }
     nonpromise_expr(2000+3000) %is% 5000
     nonpromise_env("hello") %is% emptyenv()
     expect_warning(nonpromise_expr(c(1000, 2000)) %is% c(1000, 2000))
     expect_warning(nonpromise_env(c(1000, 2000)) %is% emptyenv())
     expect_error(nonpromise_expr(quote(hello)))
     expect_error(nonpromise_env(quote(2+2)))
})

test_that("is_promise and is_forced and is_literal", {
  try <- function(f, f_, a, b, c, cmp, d) {
    d <- (c)
    f(a, b, c, d) %is% cmp
    f_(c("a", "b", "c", "d"), environment()) %is% cmp
  }
  test <- function(f, f_, a, b, c, d) {
    d <- (c)
    list(f(a, b, c, d),
         f_(c("a", "b", "c", "d"), environment()))
  }
  # a is source literal
  # b is lazy unforced
  # c is lazy forced
  # d is not lazy (so forced) or could be literal
  {
    function() test(is_promise, is_promise_, 1000, 10+10, 10+10,
                   c(FALSE, TRUE, TRUE, FALSE))
  }()
  {
    function() test(is_forced, is_forced_, 1000, 10+10, 10+10,
                   c(TRUE, FALSE, TRUE, TRUE))
  }()
  {
    function() test(is_literal, is_literal_, 1000, 10+10, 10+10,
                   c(TRUE, FALSE, FALSE, TRUE))
  }()
})

(function () {
  f <- function(x) arg_promise(x)
  f(100)
})()

test_that("empty arguments return missing value and empty environment", {
  f1 <- function(x) arg_env(x)
  f2 <- function(x) arg_expr(x)
  expect_warning(expect_identical(f1(), emptyenv()), "missing");
  expect_identical(f2(), missing_value())
})

test_that("get dotslists of args direct", {
  f1 <- function(x, y) arg_dots(x, b=y)
  d <- f1(x=one.arg, two.arg)
  names(d) %is% c("", "b")
  expressions(d) %is% alist(one.arg, b=two.arg)
  expect_identical(environments(d), list(environment(), b=environment()))
})

test_that("get dotslist of args by name", {
  f1 <- function(x, y) arg_dots_(c("x", b="y"), environment())
  d <- f1(x=one.arg, two.arg)
  names(d) %is% c("", "b")
  exprs(d) %is% alist(one.arg, b=two.arg)
  expect_identical(environments(d), list(environment(), b=environment()))
})

test_that("get dotslists handles missing arguments", {
  f1 <- function(x, y) arg_dots(x, b=y)
  d <- f1(, two.arg)
  is.missing(expressions(d)) %is% c(TRUE, b=FALSE)
  expect_identical(environments(d), list(emptyenv(), b=environment()))
})

test_that("error when symbol is not bound", {
  f <- function(x) arg_env(yweqr)
  expect_error(f(), "not")
  f <- function(x) arg_expr(yqwer)
  expect_error(f(), "not")
  f <- function(x) arg_dots(yafsd)
  expect_error(f(), "not")
  f <- function(x) missing_("yyyyy", environment())
  expect_error(f(), "not")
})

test_that("missing_", {
  f <- function(a, b, c, d, e) missing_(c("a", "b", "c", "d", "e"), environment())
  g <- function(a, b, c, d, e) f(a, b, c, d, e)
  x <- 10
  y <- missing_value()
  f( , 10, x, y, (y)) %is% c(TRUE, FALSE, FALSE, TRUE, FALSE);
  g( , 10, x, y, (y)) %is% c(TRUE, FALSE, FALSE, TRUE, FALSE);
})

test_that("R_MissingValue bound directly", {
  x <- missing_value()
  expect_warning(
   (function(x) arg_env(x))() %is% emptyenv(),
   "missing")
  expect_true(is.missing(
    (function(x) arg_expr(x))() ))
  expect_true(is.missing(
    (function(x) arg_dots(x))() ))
  expect_true(is_literal(x))
})

test_that("getting promises handles DDVAL (..1 etc)", {
  brace <- function(...) {
    e <- arg_env(..1)
    f <- arg_expr(..2)
    do.call(`{`, list(...), envir=e)
  }

  x <- 1
  y <- quote(x+3)
  brace(x, y) %is% 4
})

all.identical <- function(list) {
  falsefalse <- environment() #unique for this invocation
  ident <- function(x, y) if (identical(x, y)) x else falsefalse
  answer <- Reduce(ident, list)
  !identical(answer, falsefalse)
}

test_that("environment to dots", {
  capture <- function(a=plan, ..., z=thingy) {
    environment()
  }

  captured <- capture(one + two, f=four, five)
  d <- env2dots(captured)

  sort(names(d)) %is% c("", "a", "f", "z")
  names(d)[[order(names(d))[[1]]]] <- "anewname"
  (expressions(d)[sort(names(d))]
   %is% alist(a=one + two, anewname=five, f=four, z=thingy))
  expect_true(all.identical(environments(d)[c("anewname", "a", "f")]))
  expect_false(identical(environments(d)[["z"]], environments(d)[["a"]]))
})

test_that("dotlist to environment", {
  got <- FALSE
  id <- function(x) {
    got <<- TRUE;
    x
  }
  a <- dots(a=one, b=two, c=three, four, five, d=id(4))
  e <- dots2env(a)
  sort(ls(e)) %is% c("a", "b", "c", "d")
  got %is% FALSE
  e$d %is% 4
  got %is% TRUE
  substitute(b+c, e) %is% quote(two + three)
  substitute(list(...), e) %is% quote(list(four, five))

  # use existing, env, appending to ...
  test <- function(a, b, ...) {
    dots2env(dots(c=five, d=six, seven, eight), environment())
  }
  e2 <- test(one, two, three, four)
  substitute(list(a, b, c, d), e2) %is% quote(list(one, two, five, six))
  substitute(list(...), e2) %is% quote(list(three, four, seven, eight))
})
