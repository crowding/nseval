context("with_caller")

`%is%` <- expect_equal

test_that("calling from somewhere up the stack", {
  fenv <- NULL
  genv <- NULL
  get <- function(x) {
    c(caller()$where,
      parent.frame()$where)
  }
  f <- function() {
    fenv <<- environment()
    where <- "f"
    get <- "nope"
    g()
  }
  g <- function() {
    genv <<- environment()
    where <- "g"
    get <- "nope"
    h()
  }
  h <- function() {
    with_caller(get, fenv)() %is% c("f", "f")
    with_caller(get, genv)() %is% c("g", "g")
  }
  f()
})

test_that("with_caller by name finds in terget env?", {
  fenv <- NULL
  genv <- NULL
  f <- function(x) {
    fenv <<- environment()
    this <- "x"
    that <- "a"
    get <- function() this
    g()
  }
  g <- function(x) {
    this <- "y"
    that <- "b"
    get <- function() that
    h()
  }
  h <- function(x) {
    get <- "nope"
    #See that names and symbols are evaluated in the called environment...
    with_caller("get", fenv)() %is% "x" 
    with_caller("get", genv)() %is% "b" 
    with_caller(quote(get), fenv)() %is% "x" 
    with_caller(quote(get), genv)() %is% "b" 
  }
  f()
})

test_that("what is function called?", {
  fenv <- NULL 
  f <- function(f) {
    fenv <<- environment()
    g()
  }
  g <- function() {
    foo <- get
    with_caller(foo, fenv)()
#    with_caller("get", fenv)()
  }
  get <- function() {
    sys.calls()
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
    parent.frame()$where %is% "h"
    caller()$where $is$ "h"
  }
  f() %is% c("h", "h")
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