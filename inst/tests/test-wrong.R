context("wrong")

# This test file demonstrates some behavior of R nonstandard evalution
# functions that I think is wrong, and that this package tries to correct.


# TODO expect_different is not working...

expect_different <- function(object, expected, ..., info=NULL, label=NULL,
                             expected.label = NULL) {
  if (is.null(label)) {
    label <- deparse(arg_expr(object))
  }
  if (is.null(expected.label)) {
    expected.label <- deparse(arg_expr(expected))
  }
  expect_that(object, different_from(expected, label = expected.label,
                                     ...), info = info, label = label)
  object
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
`%is%` <- expect_equal

test_that("parent_frame returns garbage when called from a promise.", {
  # tediously explained example
  # self=invoking function declaration for reasons.
#  (function() {
    # A problem with using parent_frame to determine a function's caller is that
    # parent.frame()'s return value can change when called at different times
    # from the same environment.

    # To illustrate this, here is a NSE function that captures an expression and
    # returns a function that evaluates that expression.
    make_callable <- function(arg, envir=parent.frame()) {
      expr <- substitute(arg)
      function() {
        print(envir)
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

    #x <- 999 # oddly somnetimes it goes for global x...
    x <<- 999 # whichever x, it should be outside the count!

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
#  })()
})

#The following test cases were borrowed from test_caller. These are all
#things that caller() gets right (or throws) where parent.frame gets
#wrong.

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

  f()$where %should_be% "f"
  g()$where %should_be% "g"
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
  e() %should_be% "e"
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
  e()()$where %should_be% "e"  #example 3
  e()()$where %should_be% "0"  #example 3
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

        parent.frame()$where %should_be% "e" # example #1
        parent.frame()$where %should_be% "f" # example #1
        eval(quote(parent.frame()))$where %should_be% "f"
        do.call("parent.frame", list())$where %should_be% "f"
        do.call("parent.frame", list(), envir=y)$where %should_be% "e"
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
    parent.frame()$where %should_be% "0"
    eval(quote(parent.frame()))$where %should_be% "0"
    eval(quote(parent.frame()), y)$where %should_be% "e" #example 2
    do.call("parent.frame", list())$where %should_be% "0" #example 3
    do.call("parent.frame", list(), envir=y)$where %should_be% "e"
    do.call("parent.frame", list(), envir=y)$where %should_be% "e"
    do.call("parent.frame", list(), envir=x)$where %should_be% "f"
  }
  h()
})
