context("with_caller")

`%is%` <- expect_equal

test_that("do_", {
  expect_identical(do(emptyenv), emptyenv())
  do_(dots(`-`, 1)) %is% -1
  f <- function(y) {
    delayedAssign("x", foo+bar)
    substitute(thing(x, y))
  }
  do_(quo(f), quo(x+y)) %is% quote(thing(foo+bar, x+y))
  do(f, quo(x+y)) %is% quote(thing(foo+bar, x+y))
})

test_that("do_ with primitives", {
  # case study of using `do` with `<-`
  # One would of course rather use 'assign' in real life
  x <- 2
  do(`<-`, dots(x, x+1))
  x %is% 3

  # `<-` fails, because calling env doesn't match left-hand-side env
  e <- new.env()
  x <- 1
  e$x <- 10
  expect_error(do(`<-`, quo(x, e), quo(x+2)), "left-hand")

  # but the RHS can be in another env
  e <- new.env()
  x <- 1
  e$x <- 10
  do(`<-`, quo(x), quo(x+2, e))
  x %is% 12
  e$x %is% 10

  # and we can do the assign in another env
  e <- new.env()
  x <- 1
  e$x <- 10
  do_(quo(`<-`, e), quo(x, e), quo(x+2))
  e$x %is% 3
  x %is% 1

  # and we can assign in detached env, trickily. Note that the primitive itself
  # is going into the call, not the name `<-`
  e <- new.env(parent=emptyenv())
  x <- 10
  expect_error( do_(quo(`<-`, e), quo(x, e), quo(x+1)), "could not find")
  do_(quo_(`<-`, e), quo(x, e), quo(x+1))
  x %is% 10
  e$x %is% 11

  # however `+` is copacetic with promises and being called from emptyenv
  x <- 3
  do_(forced_quo_(`+`), dots(x+1, x+2)) %is% 9
  do_(quo(`+`, force=TRUE), dots(x+1, x+2)) %is% 9

  mode(do(alist, forced_quo_(as.name("x")))[[1]]) %is% "promise" #???
  #FIXME. This may point to an incompatibility with how we do do()
  #versus sys.call(), which makes sys.call() leak promsxps to
  #user. However, this package's position is that sys.call() is
  #broken, so the question is if we should care about sys.call()
  #leaking promises. An alternative might be to involve "...", and
  #implement do() in such a way that sys.call() always gets you
  #f(...).

})


test_that("do_", {
  # having extablished how to invoke `<-`,
  # let's make an "update" operator, like `<<-` except it starts
  # search in the present environment, but still defaulting to global
  # (that is, like Javascript "=")
  `:=` <- function(lval, rval) {
    lval_ <- arg(lval)
    rval_ <- arg(rval)
    target.env <- locate_(lval_, ifnotfound = globalenv())
    if (identical(target.env, emptyenv()))
      target.env <- globalenv()
    env(lval_) <- target.env
    do_(quo_(`<-`, target.env), lval_, rval_)
  }

  local({x <- 13; x := x + 1}) %is% 14

  x <- 5
  y <- 10

  local({
    x <- 13
    x := x + 1
    y := y + 1
    z := x + y
    x %is% 14
  })
  x %is% 5
  y %is% 11
  z %is% 25
})

test_that("`do` allows different args to come from different environments, just like ...", {
  f <- function(...) {
    here <- "f"
    g(here, ...)
  }
  g <- function(...) {
    here <- "g"
    h(here, ...)
  }
  here <- "top"

  h <- c
  f(here) %is% c("g", "f", "top")

  h <- function(...) {
    match.call() %is% quote(h(here, ..1, ..2)) #huh?
    c(...)
  }
  f(here) %is% c("g", "f", "top")

  # and our "do" can cope with different arguments having different
  # arguments.
  h <- function(...) {
    do(c, dots(...))
  }
  f(here) %is% c("g", "f", "top")

  # even doing the call from a fourth env
  e <- list2env(list(here="no"))
  h <- function(...) {
    do_(quo(c, e), dots(...))
  }
  f(here) %is% c("g", "f", "top")
})

test_that("Do passes along args", {
  f <- function(...) {
    here <- "f"
    g(here, ...)
  }
  g <- function(...) {
    here <- "g"
    h(here, ...)
  }
  here <- "top"
  e <- list2env(list(here="no"))
  h <- function(...) do_(quo(c, e), dots(...))

  f(here) %is% c("g", "f", "top")
})

test_that("calling from somewhere up the stack", {
  fenv <- NULL
  genv <- NULL
  get <- function(expected) {
    parent.frame()$where %is% expected
    caller()$where %is% expected
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
    do_(quo(get, fenv), quo("f"))
    do_(quo(get, genv), quo("g"))
  }
  f()
})

test_that("do by name finds in target env", {
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
    genv <<- environment()
    this <- "y"
    that <- "b"
    get <- function() that
    h()
  }
  h <- function(x) {
    get <- "nope"
    do_(quo(get, fenv)) %is% "x"
    do_(quo(get, genv)) %is% "b"
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
    expect_error(do_(quo(foo, fenv)))
    expect_equal(do_(quo(get, fenv)), quote(get))
  }
  get <- function() {
    match.call()[[1]]
  }
  f()
})

test_that("do down the stack in closed env", {
  where <- "0"
  f <- function() {
    where <- "f"
    henv <- g()
    do_(quo(get, henv))
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
    caller()$where %is% "h"
  }
  f()
})

test_that("do from de novo env.", {
  f <- function() {
    where <- "f"
    e <- new.env()
    e$where <- "e"
    do_(quo(get, e), quo("e"))
  }
  get <- function(expected) {
    parent.frame()$where %is% expected
    caller()$where %is% expected
  }
  f()
})

test_that("arg_envs propagate through do()", {
  where <- "0"
  eenv <- NULL
  e <- function() {
    where <- "e"
    eenv <<- environment()
    f(where)
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
    x <- do_(quo(get, eenv), dots(...))
  }
  get <- function(x, y, z) {
    caller()$where %is% "e"
    arg_env(x)$where %is% "g"
    arg_env(y)$where %is% "f"
    arg_env(z)$where %is% "e"
  }
  e()
})
