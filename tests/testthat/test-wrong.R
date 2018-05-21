context("wrong")

# This test file demonstrates some behavior of R nonstandard evalution
# functions that I think is wrong, and that this package tries to correct.

expect_different <- function(object, expected, ...) {
  lab_act <- deparse(arg_expr(object));
  lab_exp <- deparse(arg_expr(expected));
  comp <- compare(object, expected, ...)
  expect(!comp$equal, sprintf("%s was equal to %s\n%s", lab_act, lab_exp, comp$message))
  invisible(object)
}

`%should_be%` <- expect_different
`%but_is%` <- expect_equal
`%is%` <- expect_equal

test_that("parent_frame returns garbage when called from a promise.", {
  # A tediously explained example.
  # A problem with using parent_frame to determine a function's caller is that
  # parent.frame()'s return value can change when called at different times
  # from the same environment.

  # To illustrate this, here is a NSE function that captures an expression and
  # returns a function that evaluates that expression.

  make_callable <- function(arg, envir=parent.frame()) {
    expr <- substitute(arg)
    function() {
      eval(expr, envir)
    }
  }

  # Say we use "make_callable" to implement a counter factory.
  make_counter <- function() {
    x <- 0 #local x for this invocation
    make_callable(x <- x + 1) # this should refer to the local x
  }

  # Since "make_callable" is invoked from a frame containing a local x,
  count <- make_counter()
  # we expect count() should update that local value, not this global value.

  #x <- 999 # oddly sometimes it goes for global x (depending on how
            # test is invoked?)
  x <<- 999 # wherever x is, it is scoped outside the count!

  # But it does.
  ct <- count()
  ct %should_be% 1 %but_is% 1000
  x %should_be% 999 %but_is% 1000

  # That is, make_callable is not capturing the correct environment of
  # its argument. it's hitting a wrong local or global x.

  # The problem is that callable's argument "envir" is lazily evaluated, so
  # "parent.frame" gets called only when the true caller has fallen off the
  # stack. And when `parent.frame` is called by forcing a promise (and the
  # original caller is no longer on the stack), it can silently do a wrong
  # thing: it returns the stack frame that occasioned the evaluation of "x",
  # and NOT the caller of make_callable.

  # A standard workaround is to force envir to evaluate before its
  # parent frame falls off the stack.
  make_callable <- function(arg, envir=parent.frame()) {
    force(envir)
    expr <- substitute(arg)
    function() eval(expr, envir)
  }
  count <- make_counter()
  x <- 999

  count() %is% 1
  x %is% 999

  # But IMO, this is WRONG! parent.frame should not change its
  # output depending on whether it is called early or late. This
  # package provides "arg_env" and "arg_expr" instead of
  # parent.frame() and substitute(). This way we get the behavior we
  # usually want.
  make_callable <- function(arg, envir=arg_env(arg)) {
    expr <- arg_expr(arg)
    function() eval(expr, envir)
  }

  # And this works as expected.
  count <- make_counter()
  x <- 999
  count() %is% 1
  x %is% 999
})

#The following test cases were borrowed from test_caller. These are all
#things that caller() gets right (or throws) where parent.frame gets
#wrong.

test_that("parent.frame from a lazy argument in a closed environment", {
  where <<- "00"
  where <- "00"
  d <- function() {
    where <- "0"
    e <- function() {
      where <- "e"
      f <- function() {
        where <- "f"
        g <- function(g) {
          where <- "g"
          function(f) g
        }
        g(parent.frame())
      }
      f()
    }
    e()()$where
  }
  d <- compiler::cmpfun(d)
  d() %should_be% "e" %but_is% "00"
})




test_that("parent.frame from eval and do.call", {
  where <- "0"
  x <- y <- z <- NULL
  e <- function() {
    where <- "e"
    x <<- environment()
    f <- function() {
      where <- "f"
      y <<- environment()
      g <- function() {
        where <- "g"
        z <<- environment()
        eval(quote(parent.frame()))$where %should_be% "f" %but_is% NULL
        eval(quote(parent.frame()), y)$where %should_be% "e" %but_is% NULL
      }
      g()
    }
    f()
  }
  e()
})

test_that("parent.frame from eval and do.call in closed environments", {
  where <<- "00"
  x <- y <- z <- NULL
  d <- function() {
    where <- "0"
    function(x) x
    e <- function() {
      where <- "e"
      x <<- environment()
      f <- function() {
        where <- "f"
        y <<- environment()
        g <- function() {
          where <- "g"
          z <<- environment()
        }
        g()
      }
      f()
    }
    e()
  }
  d <- compiler::cmpfun(d)
  d()
  h <- function() {
    eval(quote(parent.frame()))$where %should_be% "0" %but_is% NULL
    do.call("parent.frame", list(), envir=z)$where %should_be% "f" %but_is% "00"
    do.call("parent.frame", list(), envir=y)$where %should_be% "e" %but_is% "00"
  }
  h()
})
