context("wrong")

# This test file demonstrates some behavior of R nonstandard evalution
# functions that I think is wrong, and that this package tries to correct.

expect_different <- function(object, expected, ..., info=NULL, label=NULL, expected.label = NULL) {
  if (is.null(label)) {
    label <- deparse(arg_expr(object))
  }
  if (is.null(expected.label)) {
    expected.label <- deparse(arg_expr(expected))
  }
  expect_that(object, different_from(expected, label = expected.label,
                                     ...), info = info, label = label)
}

different_from <- function(expected, label=NULL, ...) {
  if (is.null(label)) {
    label <- deparse(arg_expr(expected))
  }
  else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  function(actual) {
    same <- compare(actual, expected, ...)
    if (same$equal) {
      expectation("failure", paste0("was equal to ", label))
    } else {
      expectation("success", "")
    }
  }
}

`%should_be%` <- expect_different
`%but_is%` <- expect_equal

find_expr <- function(x, envir = caller()) {
  arg_expr_(x, envir)
}

test_that("parent_frame returns garbage when called from a promise.", {
  #tediously explained example
  #self=invoking function declaration for reasons.
  (function() {
    # A problem with using parent_frame to determine a function's caller is that
    # parent.frame()'s return value can change when called at different times
    # from the same environment.

    # To illustrate this, here is a NSE function that captures an expression and
    # returns a function that evaluates that expression.
    make_callable <- function(arg, envir=parent.frame()) {
      expr <- substitute(arg)
      function() eval(expr, envir)
    }

    # Say we use "callable" to write "make_counter."
    make_counter <- function() {
      x <- 0 #this x should change.
      make_callable(x <- x + 1)
    }

    # Since "make_callable" is invoked from a frame containing a local x,
    count <- make_counter()
    # we expect count() should update that local value, not this global value.
    x <<- 999 #this x should not change!

    # Does it? As it turns out, no.
    expect_equal(count(), 1000)
    expect_equal(x, 1000) #The wrong x changed.

    # The problem is that callable's argument "envir" is lazily evaluated, so
    # "parent.frame" gets called only when the true caller has fallen off the
    # stack. And when `parent.frame` is called by forcing a promise (and the
    # original caller is no longer on the stack), it can silently do a wrong
    # thing: it returns the stack frame that occasioned the evaluation of "x",
    # and NOT the caller of make_callable.

    # A standard workaround is to force envir to evaluate before its parent frame
    # falls off the stack.
    make_callable <- function(arg, envir=parent.frame()) {
      force(envir)
      expr <- substitute(arg)
      function() eval(expr, envir)
    }
    count <- make_counter()
    x <- 999
    expect_equal(count(), 1)
    expect_equal(x, 999) #that's more like it.

    # But IMO, this is WRONG! parent.frame should not change its output depending
    # on whether it is called early or late. This package provides
    # "arg_env" and "arg_expr" instead of parent.frame() and substitute(). This
    # way we get the behavior we usually want.
    make_callable <- function(arg, envir=arg_env(arg)) {
      expr <- arg_expr(arg)
      function() eval(expr, envir)
    }
    # And this works as expected.
    count <- make_counter()
    x <<- 999
    expect_equal(count(), 1)
    expect_equal(x, 999)
  })()
})

#following test cases were borrowed from test_caller. These are all things that caller() gets right
#(or throws) where parent.frame gets wrong.
test_that("parent.frame finds parent.frame", ({
  f1 <- function() {
    where <- "1"
    g()
  }

  f2 <- function() {
    where <- "2"
    g()
  }

  g <- function() {
    parent.frame()
  }

  f1()$where %but_is% "1"
  f2()$where %but_is% "2"
}))

test_that("parent.frame defaults to environment called from", {
  f <- function() {
    where <- "f"
    h()
  }

  g <- function() {
    where <- "g"
    h()
  }

  h <- function() {
    parent.frame()
  }

  f()$where %but_is% "f"
  g()$where %but_is% "g"
})

test_that("parent.frame from a lazy argument", {
  #baseenv calls "e" which calls "f" which calls "g"
  #"parent.frame" is written in the context of "f" so it should return "e"
  e <- function() {
    where <- "e"
    f <- function() {
      where <- "f"
      g <- function(e) {
        where <- "g"
        as.list(e)$where
      }
      g(parent.frame())
    }
    f()
  }
  e() %but_is% "e"
})

test_that("parent.frame from a lazy argument in a closed environment", {
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
  e()() %but_is% "e"  #example 3
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

        parent.frame()$where %but_is% "f" # example #1
        eval(quote(parent.frame()))$where %but_is% "f"
        do.call("parent.frame", list())$where %but_is% "f"
        do.call("parent.frame", list(), envir=y)$where %but_is% "e"
      }
      g()
    }
    f()
  }
  e()
})

test_that("parent.frame from eval and do.call in closed environments", {
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
      }
      g()
    }
    f()
  }
  e()
  h <- function() {
    parent.frame()$where %but_is% "0"
    eval(quote(parent.frame()))$where %but_is% "0"
    eval(quote(parent.frame()), y)$where %but_is% "e" #example 2
    do.call("parent.frame", list())$where %but_is% "0" #example 3
    do.call("parent.frame", list(), envir=y)$where %but_is% "e"
    do.call("parent.frame", list(), envir=y)$where %but_is% "e"
    do.call("parent.frame", list(), envir=x)$where %but_is% "f"
  }
  h()
})
