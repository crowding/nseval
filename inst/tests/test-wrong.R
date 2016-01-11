context("wrong")

# This test suite bemonstrates some behavior of traditional R nonstandard evalution 
# that I think is wrong, and that this package gets right.

test_that("parent_frame returns garbage when called from a promise.", {
  #self=invoking function declaration for reasons.
  (function() {
    # A problem with using parent_frame to determine your caller is that parent.frame()'s 
    # return value can change over time.
    # To illustrate this, here is a NSE function that captures an expression and returns
    # a function that evaluates that expression.
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
    # "parent.frame" gets called only when the true caller has fallen off the stack.
    # And when `parent.frame` is called by forcing a promise (and the original caller is no longer on the stack),
    # it can silently do a wrong thing: it returns the stack frame that occasioned the evaluation of "x", and
    # NOT the caller of make_callable.
    
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
    # on whether it is called early or late. This package reccommends you use
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


