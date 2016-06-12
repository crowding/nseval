context("wrong")

# This test file demonstrates some behavior of R nonstandard evalution 
# functions that I think is wrong, and that this package tries to correct.

expect_different <- function(object, expected, ..., info=NULL, label=NULL, expected.label = NULL) {
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  if (is.null(expected.label)) {
    expected.label <- testthat:::find_expr("expected")
  }
  expect_that(object, different_from(expected, label = expected.label, 
                                     ...), info = info, label = label)
}

different_from <- function(expected, label=NULL, ...) {
  if (is.null(label)) {
    label <- find_expr(NULL)
  }
  else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  function(actual) {
    same <- compare(actual, expected, ...)
    expectation(!same$equal, paste0("was actually equal to ", label, 
                                    "\n", same$message), paste0("different_from ", label))
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

#following test cases were borrowed from test_caller.

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
    parent.frame(environment())
  }
  
  f1()$where %should_be% "1"
  f2()$where %should_be% "2"
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
  
  f()$where %should_be% "f"
  g()$where %should_be% "g"
})

test_that("parent.frame of not the immediate environment", {
  where <- "e"
  f <- function() {
    where <- "f"
    a <- environment()
    g(a)
  }
  g <- function(a) {
    where <- "g"
    b <- environment()
    h(a, b)
  }
  h <- function(a, b) {
    where <- h
    c <- environment()
    parent.frame(c)$where %should_be% "g"
    parent.frame(b)$where %should_be% "f"
    parent.frame(a)$where %should_be% "e"
  }
  f()
})

test_that("parent.frame of a closed environment (contra parent.frame)", {
  where <- "0"
  
  f <- function() {
    where <- "f"
    g()
  }
  
  g <- function() {
    where <- "g"
    environment()
  }
  
  parent.frame(g())$where %should_be% "f"
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
  e()() %should_be% "e"  #example 3
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
        
        parent.frame()$where %should_be% "f" # example #1
        parent.frame(y)$where %should_be% "e"
        eval(quote(parent.frame()))$where %should_be% "f"
        eval(quote(parent.frame()), y)$where %should_be% "e" 
        do.call("parent.frame", list())$where %should_be% "f"
        do.call("parent.frame", alist(z))$where %should_be% "f" 
        do.call("parent.frame", alist(y))$where %should_be% "e"
        do.call("parent.frame", list(), envir=y)$where %should_be% "e"
        do.call("parent.frame", alist(x), envir=y)$where %should_be% "0"
        do.call("parent.frame", list(z), envir=x)$where %should_be% "f"
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
    parent.frame(y)$where %should_be% "e"
    eval(quote(parent.frame()))$where %should_be% "0"
    eval(quote(parent.frame()), y)$where %should_be% "e" #example 2
    do.call("parent.frame", list())$where %should_be% "0" #example 3
    do.call("parent.frame", alist(z))$where %should_be% "f" 
    do.call("parent.frame", alist(y))$where %should_be% "e"
    do.call("parent.frame", envir=y)$where %should_be% "e"
    do.call("parent.frame", alist(x), envir=y)$where %should_be% "e"
    do.call("parent.frame", list(z), envir=x)$where %should_be% "f"
  }
  h()
})




