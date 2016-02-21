context("caller")

`%is%` <- expect_equal

test_that("Caller finds caller", {
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
})

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
  
  caller(g())$where %is% "f"
})

test_that("caller from a lazy argument", {
  #baseline calls "e" which calls "f" which calls "g"
  #"caller" is written in the context of "f" so it should return "e"
  e <- function() {
    where <- "e"
    f <- function() {
      where <- "f"
      g <- function(e) {
        where <- "g"
        list(e)[[1]]
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
  e()() %is% "e"
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
        
        caller()$where %is% "f"
        caller(y)$where %is% "e"
        eval(quote(caller())) %is% "f"
        eval(quote(caller()), y)$where %is% "e" 
        do.call("caller", list()) %is% "f"
        do.call("caller", alist(z)) %is% "f" 
        do.call("caller", alist(y)) %is% "e"
        do.call("caller", envir=y) %is% "e"
        do.call("caller", alist(x), envir=y) %is% "e"
        do.call("caller", list(z), envir=x) %is% "f"
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
  caller()$where %is% "f"
  caller(y)$where %is% "e"
  eval(quote(caller())) %is% "f"
  eval(quote(caller()), y)$where %is% "e" 
  do.call("caller", list()) %is% "f"
  do.call("caller", alist(z)) %is% "f" 
  do.call("caller", alist(y)) %is% "e"
  do.call("caller", envir=y) %is% "e"
  do.call("caller", alist(x), envir=y) %is% "e"
  do.call("caller", list(z), envir=x) %is% "f"
})
