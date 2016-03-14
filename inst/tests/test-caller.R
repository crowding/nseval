context("caller")

expect_throws_if_isnt <- function (object, expected, ..., info = NULL, label = NULL, expected.label = NULL) 
{
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  if (is.null(expected.label)) {
    expected.label <- testthat:::find_expr("expected")
  }
  expect_that(object, throws_if_isnt(expected, label = expected.label, 
                                     ...), info = info, label = label)
}

throws_if_isnt <- function(expected, regexp = NULL, label=NULL, ...) {
  if (is.null(label)) {
    label <- testthat:::find_expr("expected")
  } else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  function(expr) {
    res <- try(force(expr), TRUE)
    no_error <- !inherits(res, "try-error")
    if (no_error) {
      same <- compare(res, expected, ...)
      if (same$equal) {
        return(testthat::expectation(FALSE, 
                           paste0("did not throw and equals ", label),
                           "threw error"));
      } else {
        return(testthat::expectation(same$equal, 
                                     paste0("not equal to ", label, "\n", same$message), 
                                     paste0("equals ", label)))
      }
    }
    if (!is.null(regexp)) {
      matches(regexp, ...)(res)
    } else {
      expectation(TRUE, "no error thrown", "threw an error")
    }
  }
}
  
`%is%` <- expect_equal
`%is*%` <- expect_throws_if_isnt

test_that("Caller finds caller", ({
  f1 <- function() {
    where <- "1"
    g()
  }
  
  f2 <- function() {
    where <- "2"
    g()
  }
  
  g <- function() {
    caller(environment())
  }
  
  f1()$where %is% "1"
  f2()$where %is% "2"
}))

test_that("caller defaults to environment called from", {
  f <- function() {
    where <- "f"
    h()
  }
  
  g <- function() {
    where <- "g"
    h()
  }
  
  h <- function() {
    caller()
  }
  
  f()$where %is% "f"
  g()$where %is% "g"
})

test_that("caller of not the immediate environment", {
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
    caller(c)$where %is% "g"
    caller(b)$where %is% "f"
    caller(a)$where %is% "e"
  }
  f()
})

test_that("caller of a closed environment (contra parent.frame)", {
  where <- "0"
  
  f <- function() {
    where <- "f"
    g()
  }
  
  g <- function() {
    where <- "g"
    environment()
  }
  
  caller(g())$where %is*% "f"
})

test_that("caller from a lazy argument", {
  #baseenv calls "e" which calls "f" which calls "g"
  #"caller" is written in the context of "f" so it should return "e"
  e <- function() {
    where <- "e"
    f <- function() {
      where <- "f"
      g <- function(e) {
        where <- "g"
        as.list(e)$where
      }
      g(caller())
    }
    f()
  }
  e() %is% "e"
})

test_that("caller from a lazy argument in a closed environment", {
  where <- "0"
  e <- function() {
    where <- "e"
    f <- function() {
      where <- "f"
      g <- function(g) {
         where <- "g"
        function(f) g
      }
      g(caller())
    }
    f()
  }
  e()() %is*% "e"  #example 3
})

test_that("caller from eval and do.call", {
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
        
        caller()$where %is% "f" # example #1
        caller(y)$where %is% "e"
        eval(quote(caller()))$where %is% "f"
        eval(quote(caller()), y)$where %is% "e" 
        do.call("caller", list())$where %is% "f"
        do.call("caller", alist(z))$where %is% "f" 
        do.call("caller", alist(y))$where %is% "e"
        do.call("caller", list(), envir=y)$where %is% "e"
        do.call("caller", alist(x), envir=y)$where %is% "0"
        do.call("caller", list(z), envir=x)$where %is% "f"
      }
      g()
    }
    f()
  }
  e()
})

test_that("caller from eval and do.call in closed environments", {
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
    caller()$where %is% "0"
    caller(y)$where %is*% "e"
    eval(quote(caller()))$where %is% "0"
    eval(quote(caller()), y)$where %is*% "e" #example 2
    do.call("caller", list())$where %is% "0" #example 3
    do.call("caller", alist(z))$where %is*% "f" 
    do.call("caller", alist(y))$where %is*% "e"
    do.call("caller", envir=y)$where %is*% "e"
    do.call("caller", alist(x), envir=y)$where %is*% "e"
    do.call("caller", list(z), envir=x)$where %is*% "f"
  }
  h()
})
