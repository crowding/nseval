context("caller")

`%||%` <- function(a, b) if (is.null(a)) b else a

expect_throws_if_isnt <- function (object, expected, ...,
                                   info = NULL, label = NULL,
                                   expected.label = NULL)
{
  act <- list(val = try(force(object), TRUE),
              lab = as.character(label %||% arg_expr(object)))
  expected <- list(val = force(expected),
                   lab = as.character(expected.label %||% arg_expr(expected)))
  if (inherits(act$val, "try-error")) {
    expect(TRUE, "An error was thrown")
  } else {
    expect(all.equal(act$val, expected$val),
           sprintf("%s not equal to %v", act$label, expected$label))
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

test_that("get_call and get_function", {
  where <- "0"
  eenv <- NULL
  fenv <- NULL
  genv <- NULL
  henv <- NULL
  e <- function() {
    where <- "e"
    eenv <<- environment()
    f(where)
  }
  f <- function(...) {
    where <- "f"
    fenv <<- environment()
    g(where, ...)
  }
  g <- function(...) {
    where <- "g"
    genv <<- environment()
    r <- h
    (r)(where, ...)
  }
  h <- function(x, y, z, ...) {
    list(get_call(), get_function())
  }
  c <- e()
  cmp <- list(dots_(alist( (r), x=where, y=where, z=where),
                    list(  genv, genv, fenv, eenv)),
              h)
  c %is% cmp
})
