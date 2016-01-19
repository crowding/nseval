context("with_caller")

`%is%` <- expect_equal

test_that("calling from", {
  f <- function() {
    where <- "f"
    get <- "nope"
    environment()
  }
  g <- function() {
    where <- "g"
    get <- "nope"
    environment()
  }
  get <- function(x) {
    caller()$where
  }
  
  with_caller(get, f()) %is% "f"
  with_caller(get, f()) %is% "g"
})

test_that("calling by name from", {
  f <- function(x) {
    this <- "x"
    that <- "a"
    get <- function() this
    environment()
  }
  g <- function(x) {
    this <- "y"
    that <- "b"
    get <- function() that
    environment()
  }
  get <- "nope"
  with_caller("get", f())() %is% "x" 
  with_caller("get", g())() %is% "b" 
  with_caller(quote(get), f())() %is% "x" 
  with_caller(quote(get), g())() %is% "b" 
})

test_that("with_caller up the stack", {
  fenv <- NULL
  where <- "0"
  f <- function() {
    where <- "f"
    fenv <<- environment()
    g()
  }
  g <- function() {
    where <- "g"
    h()
  }
  h <- function() {
    where <- "h"
    with_caller(get, fenv)() %is% "f"
  }
  get <- function() {
    caller()$where
  }
  f()
})

test_that("with_caller down the stack in closed env", {
  where <- "0"
  f <- function() {
    where <- "f"
    henv <- g()
    with_caller(get, henv)()
  }
  g <- function() {
    where <- "g"
    h()
  }
  h <- function() {
    where <- "h"
    environment()
  }
  get <- function() {
    caller()$where
  }
  f() %is% "f"
})

test_that("with_caller from de novo env.", {
  f <- function() {
    where <- "f"
    e <- new.env()
    e$where <- "e"
    e
    with_caller(get, e) %is% "e"
    with_caller(get2, e) %is% emptyenv()
  }
  get <- function() caller()$where
  get2 <- function() caller(caller())
  f()
})

test_that("arg_envs propagate through with_caller()", {
  where <- "0"
  eenv
  e <- function() {
    where <- "e"
    f()
  }
  f <- function(...) {
    where <- "f"
    g(where, ...)
  }
  g <- function(...) {
    where <- "g"
    h(where, ...)
  }
  h <- function(...) {
    with_caller(get, eenv)(...)
  }
  get <- function(x, y, z) {
    caller()$where %is% "e" 
    arg_env(x)$where %is% "f"
    arg_env(y)$where %is% "g"
    arg_env(z)$where %is% "h"
  }
})