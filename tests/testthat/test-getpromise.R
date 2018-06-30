context("Promise extraction")

`%is%` <- expect_equal

test_that("can recover environments of arguments", {
  f1 <- function(a, ...) { #a=one@top, two@top
    b <- 1
    where = "f1"
    f2(b, where, ...) #b, where, two
  }
  f2 <- function(a, ...) { # a=b@f1, where@f1, two@top
    b <- 2
    where <- "f2"
    f3(b, where, ...) #b@f2, where@f2, where@f1, two@top
  }
  f3 <- function(a, b, c, ...) { #a=b@f2, b=where@f2, c=where@f1, two@top
    arg_expr(a) %is% quote(b)
    arg_expr(b) %is% quote(where)
    arg_expr(c) %is% quote(where)
    exprs(dots(...)) %is% alist(two)
    arg_env(a)$where %is% "f2"
    arg_env(b)$where %is% "f2"
    arg_env(c)$where %is% "f1"
    arg_env(c)$where %is% "f1"
    envs(dots(...))[[1]]$where %is% "top"
  }
  where <- "top"
  f1(one, two)
})

test_that("arg_env error on forced promise", {
  f1 <- function(x) arg_env(x)
  f2 <- function(...) {
    list(...)
    f1(...)
  }
  expect_warning(f2(12+12), "forced")
  expect_equal(f2(124), emptyenv())
})

test_that("arg_expr should not force promise", {
  e <- environment()
  # Ugh, when testthat fails here it runs an as.list.environment on
  # the environment...
  f <- function(x) {
    expect_equal(arg_expr(x), quote(y+z))
    expect_identical(arg_env(x), e)
    expect_equal(arg_expr(x), quote(y+z))
    expect_identical(arg_env(x), e)
  }
  f(y+z)
})

test_that("arg_expr and arg_env fudge when could have been literal.", {
  # R will usually do a small optimization by not bothering to
  # construct promises for arguments that are a literal in the
  # source. Therefore we will have to allow these cases with arg_expr
  # and arg_env -- returning emptyenv when it is safe to do so.
  e <- environment()

  normal <- function(x) {
    list(arg_expr(x), arg_env(x))
  }
  expect_identical(normal(2000+3000), list(quote(2000+3000), environment()))

  # force optimization of literals
  f <- (function() normal(5000))
  f <- compiler::cmpfun(f)
  expect_identical(f(), list(5000, emptyenv()))
})

test_that("arg_expr and arg_env when expression is already forced.", {
  # But when the promise is forced?
  force_then_expr <- function(x) {
    force(x)
    arg_expr(x)
  }
  force_then_env <- function(x) {
    force(x)
    arg_env(x)
  }
  force_then_expr(2000+3000) %is% quote(2000+3000)
  expect_warning(force_then_env(2000+3000) %is% emptyenv(), "forced")
  force_then_expr(5000) %is% 5000 # not a promise
  force_then_env(5000) %is% emptyenv() #not a promise
  force_then_expr(5000L) %is% 5000L #not a promise
  force_then_env(5000L) %is% emptyenv() #not a promise
  force_then_expr(quote(x)) %is% quote(quote(x)) #language object
  expect_warning(force_then_env(quote(x)), "forced")
})

test_that("arg_expr and arg_env when expression is not a promise", {
  # and what about bindings that are not promises?
     nonpromise_expr <- function(x) {
       y <- x
       arg_expr(y)
     }
     nonpromise_env <- function(x) {
       y <- x
       arg_env(y)
     }
     nonpromise_expr(2000+3000) %is% 5000
     nonpromise_env("hello") %is% emptyenv()
     expect_warning(nonpromise_expr(c(1000, 2000)) %is% c(1000, 2000))
     nonpromise_env(c(1000, 2000)) %is% emptyenv()
     expect_warning(nonpromise_expr(quote(hello)) %is% quote(quote(hello)))
     expect_warning(nonpromise_env(quote(2+2)) %is% emptyenv())
})

test_that("is_promise and is_forced and is_literal and is_missing", {

  # a is source literal (when running from testthat/compiled function)
  # b is lazy unforced
  # c is lazy forced (as well as not function-mode)
  # d (not an argument) is not lazy (so forced) or could be literal
  # e is missing
  dbg <- function(f, f_,
                  a, b, c, e) {
    d <- (c)
    list(f(a, b, c, d, e),
         f("a", "b", "c", "d", "e"),
         f_(c("a", "b", "c", "d", "e"), environment()),
         f_(alist(a, b, c, d, e), environment()))
  }

  both <- function(data, cmp) {
    ccll <- match.call()
    force(data)
    withCallingHandlers({
      expect_equal(data[[1]], cmp)
      expect_equal(data[[2]], cmp)
      expect_equal(data[[3]], cmp)
      expect_equal(data[[4]], cmp)
    }, error=function(e) {
      message(deparse(ccll))
      message(deparse(data[[1]]))
      message(deparse(data[[2]]))
      message(deparse(data[[3]]))
      message(deparse(data[[4]]))
      message(deparse(cmp))
      e
    })
  }

  x <- function() {
    both(dbg(is_missing, is_missing_, 1000, 10+10, 10+10, ),
         c(a=FALSE, b=FALSE, c=FALSE, d=FALSE, e=TRUE))

    both(dbg(is_promise, is_promise_, 1000, 10+10, 10+10, ),
         # the first FALSE is TRUE when not compiled
         c(a=FALSE, b=TRUE, c=TRUE, d=FALSE, e=FALSE))

    both(dbg(is_forced, is_forced_, 1000, 10+10, 10+10, ),
         c(a=TRUE, b=FALSE, c=TRUE, d=TRUE, e=FALSE))

    both(dbg(is_literal, is_literal_, 1000, 10+10, 10+10, ),
         c(a=TRUE, b=FALSE, c=FALSE, d=TRUE, e=TRUE))
  }

  #force inlining literals
  x <- compiler::cmpfun(x)
  x()
})

test_that("unfound var", {
  expect_error(is_forced(dd5), "not found")
})

test_that("arg_get from promises", {
  set_arg(x, quo(4, environment()))
  is_promise(x) %is% c(x = TRUE)
  is_literal(x) %is% c(x = TRUE)
  is_forced(x) %is% c(x = FALSE)

  set_arg(x, quo_(c("a", "a"), environment()))
  is_promise(x) %is% c(x = TRUE)
  is_literal(x) %is% c(x = FALSE)
  is_forced(x) %is% c(x = FALSE)

  set_arg(x, forced_quo_(c("a", "a")))
  is_promise(x) %is% c(x = TRUE)
  is_literal(x) %is% c(x = FALSE)
  is_forced(x) %is% c(x = TRUE)

  set_arg(x, forced_quo_(c("a")))
  is_promise(x) %is% c(x = TRUE)
  is_literal(x) %is% c(x = TRUE)
  is_forced(x) %is% c(x = TRUE)

  # FIXME: since this doesn't arise normally, perhaps set_arg should
  # translate missing promises back to pure missings (also forced
  # literals, etc.)
  set_arg(x, forced_quo_(missing_value()))
  is_promise(x) %is% c(x = TRUE)
  is_literal(x) %is% c(x = FALSE) #this doesn't arise normally?
  is_missing(x) %is% c(x = TRUE)
  arg_expr(x) %is% quote(quote())

  a <- 5
  set_arg(x, force_(quo(a)))
  arg_expr(x) %is% quote(a)
  is_promise(x) %is% c(x = TRUE)
  is_literal(x) %is% c(x = FALSE)
  is_forced(x) %is% c(x = TRUE)
  is_missing(x) %is% c(x = FALSE)
  expect_warning(arg_env(x) %is% emptyenv(), "forced")

  set_arg(x, forced_quo_(identity))
  identical(arg_expr(x), identity)
  is_promise(x) %is% c(x = TRUE)
  is_literal(x) %is% c(x = FALSE)
  is_forced(x) %is% c(x = TRUE)
  is_missing(x) %is% c(x = FALSE)

  xx <- arg(x)
  forced(xx) %is% TRUE

  x <- quote(x)
  expect_warning(arg(x), "promise")
  set_arg(x, forced_quo_(c(3, 4)))
  expect_warning(arg_env(x), "forced")
  is_missing(x) %is% c(x = FALSE)

  x <- list(x)
  expect_warning(arg(x), "promise")
  expect_warning(arg(x), "promise")
  expect_warning(arg(x), "promise")

  dots2env(dots(a, b))
})


test_that("get_arg when var has a non-promise expression", {
  x <- quote(y)
  expect_warning(expect_identical(arg_env(x), emptyenv()), "promise")
  expect_warning(expect_identical(arg_expr(x), quote(quote(y))), "promise")
  is_literal(x) %is% c(x = FALSE)
  is_forced(x) %is% c(x = TRUE)
  is_missing(x) %is% c(x = FALSE)
})

test_that("empty arguments return missing value and empty environment", {
  f1 <- function(x) arg_env(x)
  f2 <- function(x) arg_expr(x)
  expect_identical(f1(), emptyenv())
  expect_identical(f2(), missing_value())
})

test_that("get dotslists of args direct", {
  f1 <- function(x, y) arg_list(x, b=y)
  d <- f1(x=one.arg, two.arg)
  names(d) %is% c("", "b")
  exprs(d) %is% alist(one.arg, b=two.arg)
  expect_identical(envs(d), list(environment(), b=environment()))
})

test_that("circular unwrap detection", {
  f <- function(a = b, b = c, c = a) {
    missing(a)
    is_missing(a)
  }
  f(c=1) %is% c(a = FALSE)
  expect_error(f())
})

test_that("args mirrors arg names by default", {
  f1 <- function(x, y) arg_list(x, y)
  d <- f1(x=one.arg, two.arg)
  names(d) %is% c("x", "y")
})

test_that("get dotslist of args by name", {
  f1 <- function(x, y) arg_list_(c("x", b="y"), environment())
  d <- f1(x=one.arg, two.arg)
  names(d) %is% c("", "b")
  exprs(d) %is% alist(one.arg, b=two.arg)
  expect_identical(envs(d), list(environment(), b=environment()))
})

test_that("get dotslists handles missing arguments", {
  f1 <- function(x, y) arg_list(x, b=y)
  d <- f1(, two.arg)
  missing_(exprs(d)) %is% c(TRUE, b=FALSE)
  expect_identical(envs(d), list(emptyenv(), b=environment()))
})

test_that("error when symbol is not bound", {
  f <- function(x) arg_env(yweqr)
  expect_error(f(), "not")
  f <- function(x) arg_expr(yqwer)
  expect_error(f(), "not")
  f <- function(x) args(yafsd)
  expect_error(f(), "not")
  f <- function(x) is_missing_("yyyyy", environment())
  expect_error(f(), "not")
})

test_that("empty dots accessors return empty lists", {
  length(dots()) %is% 0
  length(dots_exprs()) %is% 0
  length(dots_envs()) %is% 0
  length(is_forced()) %is% 0
  length(is_missing()) %is% 0
  length(is_literal()) %is% 0
  length(is_promise()) %is% 0
  length(forced(arg_list())) %is% 0
  length(missing_(arg_list())) %is% 0
})

test_that("get args by character", {
  f <- function(...) {
    arg("...")
  }
  expect_error(f())

  ff <- function(a, b, what) {
    arg_(what)
  }
  expr(ff(foo, bar, "b")) %is% quote(bar)
  expect_error(ff(foo, bar, "..."))

  g <- function(a, b, ...) {
    arg_list_(c("a", "b", "..."), environment())
  }

  exprs(g(a=foo, c=baz, q=quux, b=bar)) %is%
    alist(a=foo, b = bar, c=baz, q=quux)

  ff <- function(x, y) arg_expr("y")
  ff(foo, bar) %is% quote(bar)
})


test_that("is_missing_ unwraps naturally created promise chains", {
  f <- function(a, b, c, d, e) {
    x <- is_missing_(c("a", "b", "c", "d", "e"), environment())
    y <- missing_(arg_list(a, b, c, d, e))
    x %is% y
    x
  }
  g <- function(...) f(...)
  h <- function(A, B, C, D, E) g(A, B, C, D, E)
  x <- 10
  y <- missing_value()
  f( , 10, x, y, (y)) %is% c(a=TRUE, b=FALSE, c=FALSE, d=TRUE, e=FALSE);
  g( , 10, x, y, (y)) %is% c(a=TRUE, b=FALSE, c=FALSE, d=TRUE, e=FALSE);
  h( , 10, x, y, (y)) %is% c(a=TRUE, b=FALSE, c=FALSE, d=TRUE, e=FALSE);
})

test_that("is_missing_ unwraps explicitly created promise chains?", {
  a <- 1
  b <- missing_value()
  e0 <- dots2env(dots(w=b, x=a, y="no", z=))
  e1 <- dots2env(dots_(alist(a=w, b=x, c=y, d=z), e0))
  target <- c(a = TRUE, b = FALSE, c = FALSE, d = TRUE)
  is_missing_(c("a", "b", "c", "d"), e1) %is% target
  missing_(arg_list_(c("a", "b", "c", "d"), e1)) %is% target
})

test_that("R_MissingValue bound directly", {
  x <- missing_value()
  (function(x) arg_env(x))() %is% emptyenv()
  expect_true(missing_( (function(x) arg_expr(x))() ))
  expect_true(missing_( (function(x) arg_list(x))() ))
  expect_true(is_literal(x))

  set_arg(x, forced_quo(missing_value()))
  is_forced(x) %is% c(x = FALSE)
})

test_that("missing_ matches R behavior with unwrapping", {
  delayedAssign("aa", )
  delayedAssign("bb", aa)
  delayedAssign("cc", asdlkhj)
  delayedAssign("dd", asdlkjh + alsiduj)

  f <- function(e,f,g,h) {
    cmp <- base::c(aa = TRUE, bb = TRUE, cc = FALSE, dd = FALSE,
                   e = missing(e), f = missing(f), g = missing(g), h = missing(h))
    is_missing.tst <- is_missing(aa, bb, cc, dd, e, f, g, h)
    missing_dots.tst <- missing_(dots(aa=aa, bb=bb, cc=cc, dd=dd, e=e, f=f, g=g, h=h))
    missing_args.tst <- missing_(arg_list(aa, bb, cc, dd, e, f, g, h))
    missing_quo.tst <- c(aa = missing_(quo(aa)),
                         bb = missing_(quo(bb)),
                         cc = missing_(quo(cc)),
                         dd = missing_(quo(dd)),
                         e = missing_(quo(e)),
                         f = missing_(quo(f)),
                         g = missing_(quo(g)),
                         h = missing_(quo(h)))
    cmp %is% is_missing.tst
    cmp %is% missing_dots.tst
    cmp %is% missing_args.tst
    cmp %is% missing_quo.tst
  }
  f(aa, bb, cc, dd)
})

test_that("getting promises handles DDVAL (..1 etc)", {
  brace <- function(...) {
    e <- arg_env(..1)
    f <- arg_expr(..2)
    do.call(`{`, list(...), envir=e)
  }

  x <- 1
  y <- quote(x+3)
  brace(x, y) %is% 4
})

test_that("ddvals", {
  x <- {function(...) arg_list(..1, ..2)}(a, b, c)
  exprs(x) %is% alist("..1" = a, "..2" = b)
})

all.identical <- function(list) {
  falsefalse <- environment() #unique sigil for this invocation
  ident <- function(x, y) if (identical(x, y)) x else falsefalse
  answer <- Reduce(ident, list)
  !identical(answer, falsefalse)
}

test_that("environment to dots", {
  capture <- function(a=plan, ..., z=thingy) {
    environment()
  }

  captured <- capture(one + two, f=four, five)
  d <- env2dots(captured)

  sort(names(d)) %is% c("", "a", "f", "z")
  names(d)[[order(names(d))[[1]]]] <- "anewname"
  (exprs(d)[sort(names(d))]
   %is% alist(a=one + two, anewname=five, f=four, z=thingy))
  expect_true(all.identical(envs(d)[c("anewname", "a", "f")]))
  expect_false(identical(envs(d)[["z"]], envs(d)[["a"]]))
})

test_that("dotlist to environment", {
  got <- FALSE
  id <- function(x) {
    got <<- TRUE;
    x
  }
  a <- dots(a=one, b=two, c=three, four, five, d=id(4))
  e <- dots2env(a)
  sort(ls(e)) %is% c("a", "b", "c", "d")
  got %is% FALSE
  e$d %is% 4
  got %is% TRUE
  substitute(b+c, e) %is% quote(two + three)
  substitute(list(...), e) %is% quote(list(four, five))

  # use existing, env, appending to ...
  test <- function(a, b, ...) {
    dots2env(dots(c=five, d=six, seven, eight), environment())
  }
  e2 <- test(one, two, three, four)
  substitute(list(a, b, c, d), e2) %is% quote(list(one, two, five, six))
  substitute(list(...), e2) %is% quote(list(three, four, seven, eight))
})

test_that("arg_expr doesn't lookup literals as if they were variables", {
  ## > (function(x) arg_expr(x))(1)
  ## Error in arg_expr_(arg_expr_(quote(name), environment()), env) (from getpromise.R#104) : 
  ##   Variable `1` was not found.
  (function(x) arg_expr(x))(1) %is% 1
})

test_that("arg_expr doesn't over-unwrap...", {
  f <- function(x) arg_expr(x)
  g <- function(x) f(x)
  h <- function(x) g(x)
  f(3) %is% quote(3)
  g(3) %is% quote(x)
  h(3) %is% quote(x)
})

test_that("locate var", {
  x <- function() {
    x <- 1
    y <- function() {
      y <- 1
      z <- function() {
        nx <- sort(names(locate(x)))
        nx_ <- sort(names(locate_(quote(x))))
        ny <- sort(names(locate(y)))
        nyf <- sort(names(locate(y, mode = "function")))
        ny_x <- sort(names(locate(y, env = locate(x))))
        nx %is% c("x", "y")
        nx_ %is% c("x", "y")
        ny %is% c("y", "z")
        nyf %is% c("x", "y")
        ny_x %is% c("x", "y")
      }
      z()
    }
    y()
  }
  x()
})


test_that("locate list", {
  xe <- environment()
  x <- function() {
    ye <- environment()
    y <- function() {
      ze <- environment()
      z <- function() {
        exyz <- list(xe, ye, ze)
        expect_error(locate_(c("x", "y", "z")), "list")
        ff <- locate_(alist(x, y, z))
        ff %is% exyz
        ll <- locate_.list(c("x", "y", "z"))
        ll %is% exyz
      }
      z()
    }
    y()
  }
  x()
})


test_that("locate dots", {
  x <- function(...) {
    y <- function() {
      i <- locate_(quote(...), environment())
      k <- locate_("...", environment())
      j <- locate( (...) )
      expect_error(locate("...", mode = "function"))
      expect_identical(i, k)
      expect_identical(j, k)
      expect_false(identical(i, environment()))
    }
    y
  }
  f <- x(a, b, c)
  f()
})

test_that("locate function, forcing in process", {
  x <- function(...) {
    y <- function(x) {
      expect_false(is_forced(x))
      locate(x, mode="function")$x %is% xx
      expect_true(is_forced(x))
    }
    y(2+2)
  }
  xx <- x
  x()
})

test_that("unwrap quotation", {
  f <- function(r, q) {
    g(r, q)
  }
  g <- function(y, q) {
    h(y, q)
  }
  h <- function(z, q) {
    q(z)
  }

  f(1 + 2, function(x) unwrap(arg(x), TRUE)) %is% quo(1+2)
  expr(f(1 + 2, function(x) unwrap(arg(x), FALSE))) %is% quote(y)
  expr(f(1 + 2, function(x) unwrap(quo(x), FALSE))) %is% quote(z)
  expr(f(1 + 2, function(x) unwrap(quo(x), TRUE))) %is% quote(1+2)
  f((400), function(x) unwrap(quo(x), TRUE)) %is% quo((400))

  ff <- function() {
    f(400, function(x) unwrap(quo(x), TRUE))
  }
  ff <- compiler::cmpfun(ff)
  expr(ff()) %is% quote(r)
})

test_that("is_default", {
  g <- function() {
    f <- function(x = "this is my default") is_default(x)
    expect_true(f())
    expect_false(f("no"))
    expect_false(f("this is my default"))
  }

  h <- compiler::cmpfun(g)
  body(g) <- body(g) # strip compilation if any
  g()
  h()

  g <- function() {
    f <- function(x = two+two) is_default(x)
    expect_true(f())
    expect_false(f("no"))
    expect_false(f(two+two))
  }

  h <- compiler::cmpfun(g)
  body(g) <- body(g)
  g()
  h()
})
