context("formatting")

`%is%` <- function(...) expect_equal(..., expected.label = ..2)

`%parses_like%` <- function(left, right)
  expect_equal(parse(text=right, keep.source=FALSE),
               parse(text=left, keep.source=FALSE))

silently <- function(x, output_callback = force) {
  y <- NULL
  output_callback(capture.output(y <- print(x)))
  y
}

test_that("dots_unpack has a print method that works", {
  x <- capture.output(as.data.frame(dots(a, b, c, d, 4, e)))
  expect_equal(length(x), 7)
})

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

  d <- dots_some_forced(4, a=list(x+2), b+1, c=3+3, "5")

  collapse_lines <- function(..., collapse=" ") {
    paste0(vapply(c(...), trimws, ""), collapse=collapse)
  }

  collapse_lines(format(d)) %is%
    paste0("c.dots(forced_quo_(4), a = forced_quo(list(x + 2), value=list(4)), ",
           "quo(b + 1, ", e, "), c = quo(3 + 3, ", e, "), quo(\"5\", ", e, "))")
  format(d, show.environments=FALSE) %parses_like%
    paste0("c.dots(forced_quo_(4), a = forced_quo(list(x + 2), value=list(4)),",
                   "quo(b + 1), c = quo(3 + 3), quo(\"5\") )")
  collapse_lines(format(d, show.expressions=FALSE)) %is%
    paste0("c.dots(forced_quo_(4), a = forced_quo_(list(4)), quo(b + 1, ", e,
           "), c = quo(3 + 3, ", e, "), quo(\"5\", ", e, "))")

  format(d[[1]]) %is% "forced_quo_(4)"
  format(d[[2]]) %is% "forced_quo(list(x + 2), value=list(4))"
  format(d[[3]]) %is% paste0("quo(b + 1, ", e, ")")
  format(d[[4]]) %is% paste0("quo(3 + 3, ", e, ")")
  format(d[[5]]) %is% paste0("quo(\"5\", ", e, ")")

  format(quo_(quote(f), globalenv())) %is%
    "quo(f, <environment: R_GlobalEnv>)"
  format(dots_(list(quote(f)), globalenv())) %is%
    "c.dots(quo(f, <environment: R_GlobalEnv>))"
  collapse_lines(format(dots(a, b, c))) %is%
    paste0("c.dots(quo(a, ", e, "), quo(b, ", e, "), quo(c, ", e, "))")
})

test_that("format output is parseable when multiline as well", {
  expect_equal(
    length(format(dots(a = function(x)x))),
    1)
  expect_equal(
    length(format(quo(function(x){x}), show.environments = FALSE)),
    3)
  expect_equal(
    length(format(forced_dots_(list(a = function(x)x)))),
    1)
  expect_equal(
    length(format(dots(a = function(x){x}))),
    3)

  format(quo(function(x){x}),
         show.environments = FALSE
         ) %parses_like% "quo(function(x) {x})"

  format(dots(a = function(x){x}),
         show.environments = FALSE
         ) %parses_like% "c.dots(a=quo(function(x) {x} ))"

  format(dots(a = function(x)x, b=function(x){x}),
         show.environments=FALSE) %parses_like%
    "c.dots(a=quo(function(x) x ), b = quo(function(x) {x}))"
})

test_that("weird promises", {

  local({
    c.boo <- function(...) {
      expect_true(is_forced(..1))
      expect_identical(env(arg(..1)), parent.env(environment()))
      expect_match(format(arg(..1)), "weird")
    }
    c(structure(list(1), class="boo"), 34)
  })

})

test_that("oneline", {
  formats <- format.oneline(list(
    "a",
    12,
    function(x) {x},
    c(1, 2),
    c(3, 4),
    quote(x),
    quote(x+y)))

  expect_true(all(formats != "?FORMAT?"))
})

expect_different <- function(a,b) expect_false(isTRUE(all.equal(a, b)))

test_that("all.equal", {
  env1 <- NULL
  env2 <- NULL
  hello <- function(x) x
  world <- 5
  there <- 5
  expect_different(quo(hello), function() hello) # diff. class
  q1 <- local({ env1 <<- environment(); quo(hello(world)) })
  q2 <- local({ env2 <<- environment(); quo(hello(world)) })
  qa <- quo(hello(world), env=env1)
  expect_equal(q1, qa)
  expect_different(q1, q2) # diff. env
  q1f <- force_(q1)
  expect_different(q1, q1f) #only one is forced
  q2f <- force_(q2)
  expect_equal(q1f, q2f) # now same env
  q3f <- quo(hello(there), force=TRUE)
  expect_different(q1f, q3f) # diff. expressions
  world <- 4
  q4f <- quo(hello(world), force=TRUE)
  expect_different(q1f, q4f) # diff. values
})
