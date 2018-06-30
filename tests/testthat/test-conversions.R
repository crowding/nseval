context("dots conversions")

`%is%` <- expect_equal
`%throws%` <- expect_error

# get the dots from an environment
test_that("get_dots", {
  f <- function(a, b, ...) {
    environment()
  }

  e <- f(a=foo, b=bar, c=baz, d=quux)
  d <- get_dots(e)
  exprs(d) %is% alist(c=baz, d=quux)

  g <- function(a, b, ...) {
    get_dots()
  }
  d <- g(a=foo, b=bar, c=baz, d=quux)
  exprs(d) %is% alist(c=baz, d=quux)

  d <- g(a=foo, b=bar)
  exprs(d) %is% list()
  length(d) %is% 0

  #get_dots when no dots binding should return empty dotlist
  f <- function(x) environment();
  length(get_dots(f())) %is% 0

  #by default we do not inherit
  f <- function(..., inherit=FALSE) {
    g <- function(inherit) {
      get_dots(environment(), inherit=inherit)
    }
    g(inherit)
  }
  length(f("nope")) %is% 0
  length(f(and = "yep", inherit=TRUE)) %is% 1
})

test_that("set_dots", {
  g <- function(x, ...) {
    list(function() exprs(dots(...)), environment())
  }
  fe <- g(a, b, foo=c)
  f <- fe[[1]]
  e <- fe[[2]]

  f() %is% exprs(dots(b, foo=c))
  set_dots(e, dots(x, y, bar=z))
  f() %is% exprs(dots(x, y, bar=z))

  set_dots(e, dots())
  f() %is% list()

  # calling set_dots adds dots binding if not already present
  # this will generate warning about ... used in incorrect context
  g <- function() {
    list(function() exprs(dots(...)), environment())
  }
  fe <- g()
  f <- fe[[1]]
  e <- fe[[2]]
  set_dots(e, dots(a, foo=b, c))
  f() %is% exprs(dots(a, foo=b, c))

  # and with append=TRUE
  f <- function(...) {
    set_dots(env=environment(), dots(a=a, b=b), append=TRUE)
    set_dots(env=environment(), dots(c=c, d=d), append=TRUE)
    names(get_dots(environment())) %is% c("a", "b", "c", "d")
    names(dots(...)) %is% c("a", "b", "c", "d")
  }

  # append to unbound
  g <- function() {
    set_dots(env=environment(), dots(a=a, b=b), append=TRUE)
    names(get_dots(environment())) %is% c("a", "b")
  }

  f()
  g()
 })

test_that("convert dots to list of closures", {
  x <- 2
  neg <- `-`
  d <- dots(4+5, n = neg(6), x * 3)
  d <- as.list(d)
  mode(d[[1]]) %is% "function"
  mode(d[[2]]) %is% "function"
  names(d) %is% c("", "n", "")
  d[[1]]() %is% 9
  d[[2]]() %is% -6
  d[[3]]() %is% 6

  neg <- `+`
  x <- 4

  d[[2]]() %is% 6
  d[[3]]() %is% 12
})

test_that("convert list of closures to dots", {
  x <- 1
  y <- 2

  fa <- function() x + y
  fb <- local({
    x <- 4
    y <- 3
    function() x * y
  })

  d <- as.dots(list(fa, fb))

  `%->%` <- function(arglist, f) do(f, arglist)

  f <- function(a, b) {
    arg_expr(a) %is% quote(x + y)
    arg_expr(b) %is% quote(x * y)
    a %is% 3
    b %is% 12
  }

  do(f, d)

  d %->% (function(a, b) {
                              arg_expr(a) %is% quote(x + y)
                              arg_expr(b) %is% quote(x * y)
                              a %is% 3
                              b %is% 12
                              })

  #invalid closures
  l <- list(function(x=5) x+1)
  as.dots(l) %throws% "arg"
  x <- list(`+`) #"formals" doesn't work on builtins
  as.dots(x) %throws% "primitive"
})

test_that("convert environment to dots", {
  f <- function(a) {
    function(b, c) {
      environment()
    }
  }
  ae <- NULL
  e <- NULL
  local({
    ae <<- environment()
    e <<- f(toupper("a"))(LETTERS[2], paste0("", "c"))
  })

  d <- as.dots(e)

  names(d) %is% c("b", "c")
  exprs(d) %is% alist(b=LETTERS[2], c=paste0("", "c"))
  envs(d) %is% list(b=ae, c=ae)
})

test_that("convert environment to dots unpacks ...", {
  abenv <- function(a, b, ...) {
    environment()
  }
  e <- abenv(a=aye, b=bee, c=see, none)

  sort(names(env2dots(e)), na.last=FALSE) %is% c("", "a", "b", "c")
  sort(names(env2dots(e, expand_dots=FALSE)), na.last=FALSE) %is% c("a", "b")
})

test_that("convert environment to dots with missings, ", {
  abenv <- function(a, b) {
    environment()
  }
  d <- env2dots(abenv(a = aye, b = ))
  dnm <- env2dots(abenv(a = aye, b = ), include_missing=FALSE)

  sort(names(d)) %is% c("a", "b")
  sort(names(dnm)) %is% c("a")
})

test_that("convert dots to environment", {
  aa <- function(f, ...) bb(..., a=letters[[1]])
  bb <- function(f, ...) cc(..., b=letters[[2]])
  cc <- function(...) dots(...)

  d <- aa(b, dots, q = arbitraryArgument, someArgument(a))

  names(d) %is% c("q", "", "a", "b")
  e <- as.environment(d)
  sort(names(e)) %is% c("...", "a", "b", "q")

  sort(ls(envir = envs(d)$a, all.names=TRUE)) %is% c("...", "f")

  substitute(list(...), e) %is% quote(list(someArgument(a)))
  substitute(q, e) %is% quote(arbitraryArgument)

  #and with specified names,
  e <- dots2env(d, names="a")
  sort(ls(e, all.names=TRUE)) %is% c("...", "a")
  names(get_dots(e)) %is% c("q", "", "b")

  # on empty dots we get empty env
  ls(envir = as.environment(dots()), all.names=TRUE) %is% character(0)

  # if we request illegal names, error out.
  expect_error(dots2env(dots(a="aye", b="bee", "nothing"), c("a", "")))
  expect_error(dots2env(dots(a="aye", b="bee", `...`="nothing"), c("...")))
  expect_error(dots2env(dots(a="aye", b="bee", "nothing"), NA))

  # and how about by requesting a name that doesn't exist.
  expect_error(dots2env(dots(a="aye", b="bee"), names=c("a", "q")))

  #and I guess dots2env has to append to existing dots too, eh.
  ab_env <- function(a, b, ...) environment()
  e <- ab_env(a=a, b=b, c=c, d=d, e=e)
  d <- dots(c=33, e=34, b=35)
  e <- dots2env(d, names="c", env=e, use_dots = TRUE, append = TRUE)

  names(get_dots(e)) %is% c("c", "d", "e", "e", "b")

  #or if use_dots is false, extra args are thrown away.
  e <- ab_env(a=a, b=b, c=c, d=d, e=e)
  d <- dots(c=33, e=34, b=35)
  e <- dots2env(d, env=e, names=c("c"), use_dots = FALSE)
  names(get_dots(e)) %is% c("c", "d", "e")
})

test_that("convert formulas to dots", {
  x <- list(~a+b)
  e <- local({
    x <<- c(x, list(~e+f))
    environment()
  })
  d <- as.dots(x)
  deparse(x)
  envs(d)[[1]] %is% environment()

  # envs(d)[[2]] %is% e
  # exprs(d) %is% alist(a+b, e+f)
})

test_that("convert lazy_dots to dots", {
  library(lazyeval)
  x <- lazy_dots(a+b)
  e <- local({
    x <<- c(x, lazy_dots(e+f))
    environment()
  })
  d <- as.dots(x)
  envs(d)[[1]] %is% environment()
  envs(d)[[2]] %is% e
  exprs(d) <- alist(a+b, c+d)
})

test_that("convert dots to lazy_dots", {
  x <- dots(a+b)
  xx <- lazy_dots(a+b)
  l <- as.lazy_dots(x)
  l %is% xx
})

test_that("convert singleton dots to quotation", {
  expect_error(as.quo(dots()))
  expect_equal(as.quo(dots(a+b)), quo(a+b))
  expect_error(as.quo(dots(a+b, c+d)))
})

test_that("convert list to quotation", {
  x <- as.quo(list(expr=quote(x), env=baseenv()))
  expr(x) %is% quote(x)
  identical(env(x), baseenv())
})

test_that("quotation to binding", {
  x <- quo(a+b)
  e <- new.env()

  set_arg(4.0, x) %throws% "double"
  set_arg(e["x"], x) %throws% "support"
  set_arg(e$y, x) %throws% "support"
  set_arg(z, x)
  arg_expr(z) %is% quote(a+b)

  set_arg_(quo(y, environment()), x)
  arg_expr(y) %is% quote(a+b)
  set_arg_(quo(zz), x)
  arg_expr(zz) %is% quote(a+b)

  set_arg_( quo("...", e <- new.env()), x) %throws% "set_dots"
  set_arg_( quo((...), f <- new.env()), x) %throws% "set_dots"
})

test_that("setting with a string", {
  set_arg_(quo("x", environment()), quo(y+1))

  y <- runif(1)
  x %is% (y+1)
})
