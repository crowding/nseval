context("dots")

library(stringr)

`%is%` <- expect_equal

unwind_protect <- function(body, unwind) {
  on.exit(unwind)
  body
}

with_setup <- function(setup=NULL, ..., teardown=NULL) {
  setup <- arg(setup)
  teardown <- arg(teardown)
  tests <- dots(...)

  for (i in 1:length(tests)) {
    value(setup)
    unwind_protect(value(tests[[i]]),
                   value(teardown))
  }
}

## DOTSXP UNPACKING --------------------------------------------------
test_that("as.data.frame.dots extracts dots information into a data frame", {
  expect_equal(nrow(as.data.frame(dots())), 0)
  #
  f <- function(...) {
    as.data.frame(dots(...))
  }
  x <- 2
  y <- 3
  di <- f(x, y = 3, z = x+y)
  env <- environment()
  #
  expect_identical(di$expr[[1]], quote(x))
  expect_identical(di$expr[[2]], quote(3))
  expect_identical(di$expr[[3]], quote(x+y))
  expect_identical(di$env[[3]], env)
  expect_identical(di$env[[3]], env)
  expect_identical(di$env[[3]], env)
  expect_identical(di$value[[1]], NULL)
  expect_identical(di$value[[2]], NULL)
  expect_identical(di$value[[3]], NULL)
  expect_identical(di$name[[1]], "")
  expect_identical(di$name[[2]], "y")
  expect_identical(di$name[[3]], "z")
})

test_that("as.data.frame.dots exposes promise behavior", {
  a <- 12
  b <- a+2
  unpack_fns <- function(...) {
    #get functions that to things to the same dotslist
    list()
    list(
      reunpack=function() as.data.frame(dots(...)),
      eval_x=function() (function(x, ...) x)(...),
      eval_all=function() list(...),
      inner_env=environment()
      )}
  outer_env <- environment()
  l <- unpack_fns(x=a, y=a+2)
  #
  du <- l$reunpack()
  expect_identical(du$value[[1]], NULL)
  expect_identical(du$env[[1]], outer_env)
  l$eval_x()
  du2 <- l$reunpack()
  expect_identical(du2$value[[1]], 12)
  expect_identical(du2$envir[[1]], NULL)
  expect_identical(du2$envir[[2]], outer_env)
  expect_identical(du2$value[[2]], NULL)
})

test_that("as.data.frame.dots descends through promise chains if necessary", {
  y <- 1
  f1_env <- NULL
  f1 <- function(...) {
    x <- 1
    f1_env <<- environment()
    getdots(y=x+1, ...)
  }
  getdots <- function(...) as.data.frame(dots(...))

  du <- f1(a=y+z)

  expect_identical(du[["a", "envir"]], environment())
  expect_identical(du[["y", "envir"]], f1_env)
  expect_identical(du[["a", "expr"]], quote(y+z))
  expect_identical(du[["y", "expr"]], quote(x+1))
})

## these should also be in reference to dots objects
test_that("dots missingness", {
  expect_equivalent(logical(0), missing_(dots()))
  local({
    with_setup(
      setup={
        if (exists("a")) stop("Please 'rm(a)' and rerun test")
        unmissing <- 1
        b <- missing_value()
        delayedAssign("e", stop("e"))
      },
      thunk <- 1,
      #actual testing in the teardown
      teardown = {
        d <- missing_(dots(   a, unmissing, c=    ,     4, d=x+y  ,     ,     e))
        expect_equal(d, c(FALSE,     FALSE, c=TRUE, FALSE, d=FALSE, TRUE, FALSE))
        #
        #And this check for missingness does not eval
        d <- dots(stop("no"), c=, stop("no"))
        expect_equal(missing_(d), c(FALSE, c=TRUE, FALSE))
        #remove...
        #browser()
        #rm(unmissing)
        #rm(b)
      })
  })
})

unmissing <- 1
missing_(quo(unmissing))

test_that("missing on non-dotlists", {
  a <- alist(1, 2, adsf, , b=, )
  missing_(a) %is% c(FALSE, FALSE, FALSE, TRUE, b=TRUE, TRUE)
  b <- c(1, 2, NA, NaN)
  missing_(b) %is% c(FALSE, FALSE, FALSE, FALSE)
  missing_() %is% TRUE
  missing_(function(x) y) %is% FALSE
  missing_(missing_value()) %is% TRUE
  missing_(quo()) %is% TRUE
})

test_that("dots elements are promises", {
  d <- dots(a=1+1, b, c=d)
  class(d[[1]]) %is% "quotation"
  class(d[[2]]) %is% "quotation"
  class(d[[3]]) %is% "quotation"
})

test_that("arg_promise a missing", {
  f <- function(x) {
    arg(x)
  }
  expect_identical(expr(f()), missing_value())
})

test_that("promise missingness", {
  x <- quo()
  y <- quo_(expr = missing_value(), env = environment())
  z <- quo(what)
  missing_(x) %is% TRUE
  missing_(y) %is% TRUE
  missing_(z) %is% FALSE
})

test_that("list_missing", {
  expect_equivalent(list_missing(1, 2, 3),
                    list(1, 2, 3)) # names may or may not be NULL

  expect_equal(list_missing(1, 2, , "three"),
               alist(1, 2, , "three"))

  expect_equal(list_missing(a="one", b=, "three"),
               alist(a="one", b=, "three"))
})

test_that("list_missing evaluates arguments in the original scopes", {
  fOne <- function(...) {
    fThree <- function(...) {
      x <- "three"
      list_missing(..., three=x)
    }
    fTwo <- function(...) {
      x <- "two"
      fThree(..., two=x)
    }
    x <- "one"
    fTwo(..., one=x)
  }

  x <- "four"
  expect_equal(fOne(four=x),
               list(four="four", one="one", two="two", three="three"))
})

test_that("dots_exprs", {
  x <- 4
  f <- function(x, ...) {exprs(dots(...))}
  f(one, two, y=x<-3) %is% alist(two, y=x<-3)
  x %is% 4
  f <- function(x, ...) {exprs(dots(...))}
  f(one, two, y=x<-3) %is% alist(two, y=x <-3)
  x %is% 4
})

test_that("dots names", {
  names(dots(a=b, b=c, d)) %is% c("a", "b", "")
})

test_that("expression mutator", local({
  #Problem here that is a function of optimization level.
  #Not sure that I can do anything about it.
  f <- function(...) {
    ## this will be called as f(20, 5)
    ## where the 5 comes from f2() and the 20 comes from f1()
    ## we change the expressions before they are evaluated, into
    ## temp1 <- 6 and temp2 <- 66
    ## which should be in turn evaluated in the scopes those
    ## arguments came from.
    ## So e1$temp1 == 6 and e2$temp2 = 66
    x <- dots(...)
    exprs(x) <- dots_exprs(temp1 <- "6", temp2 <- "66")
    do(list, x)
    as.data.frame(x)
  }
  e1 <- NULL
  e2 <- NULL
  f1 <- function(...) {
    where <- "f1"
    temp1 <- "40"
    temp2 <- "30"
    e1 <<- environment()
    f(("20"), ...) #note parens to stop optimization
  }
  f2 <- function(...) {
    where <- "f2"
    temp1 <- "2"
    temp2 <- "3"
    e2 <<- environment()
    x <- f1(("5"), ...) #note parens to stop optimization
  }
  test <- f2()

  e1$temp1 %is% "6"
  e2$temp2 %is% "66"

  #it is NOT an error to set expressions for fulfilled quos?
  forced <- function(...) {list(...); dots(...)}
  r <- 3
  x <- forced(r+2)
  y <- dots(r+2)
  #forced quos get emptyenv as their environment
  exprs(x) <- alist(r+1)
  #which means value cannot be obtained.
  expect_error(value(x[[1]]), "could not find")
  exprs(y) <- alist(r+1)
  value(y[[1]]) %is% 4
}))

test_that("dots_envs and mutator", local({
  expect_equivalent(envs(dots()), list())
  f1 <- function(...) {
    where <- "e1E"
    f2(..., toupper(where))
  }
  f2 <- function(...) {
    where <- "e2E"
    f(..., tolower(where))
  }
  f <- function(..., accessor=dots) {
    accessor(...)
  }

  test <- f1()
  envs(test)[[1]]$where %is% "e1E"
  envs(test)[[2]]$where %is% "e2E"

  test <- f1(accessor=function(...) envs(dots(...)))
  test[[1]]$where %is% "e1E"
  test[[2]]$where %is% "e2E"

  test <- f1()
  envs(test) <- rev(envs(test))
  value(test) %is% list("E2E", "e1e")
}))

## FIXME: this passes from testthat, and if pasted at the R toplevel, but fails
# when invoked via ESS, with the error:
# `* namespace found within global environments`
#
test_that("expressions unpacks bytecode", {
  f <- function(x) dots(y=x+1)
  f <- compiler::cmpfun(f)
  exprs(f(5)) %is% alist(y=x+1)
})

test_that("dots_exprs", {
  a <- dots_exprs(a, b, d=c, d, e)
  f <- function(a, b, ...) dots_exprs(a+b, ...)
  b <- f(x, y, z, foo, wat=bar)
  expect_equal(a, alist(a, b, d=c, d, e))
  expect_equal(b, alist(a+b, z, foo, wat=bar))
})

test_that("dots_exprs is pointer-stable", {
  f <- function() {
    x <- dots_exprs(a, c+d)
    x <- str_match(capture.output(.Internal(inspect(x))),
                   "^  @([0-9a-f]*) 06 langsxp")[,2]
    x[!is.na(x)]
  }
  expect_equal(f(), f())
})

## DOTS OBJECT, CALLING AND CURRYING -------------------------------------

test_that("do with forced quos -- like do.call(quote=TRUE) without overquoting", {
  x <- 2
  y <- 5
  ff <- function(x, y) list(substitute(x), substitute(y))
  do(list, forced_dots_(list(x, y))) %is% list(2,5)
  do(list, forced_dots_(alist(x, y))) %is% ff(x, y)
  do(list, forced_dots_(ff(x, y+z))) %is% ff(x, y+z)
  do(list, forced_dots_(ff(x, y))) %is% ff(x, y)
  do(ff, forced_dots(x, y)) %is% alist(x, y)
  do(ff, forced_dots_(list(x, y))) %is% list(2, 5)
})

test_that("x <- dots() captures dots and do() calls with dots", {
  x <- 1;
  y <- 3;
  f <- `/`
  d <- dots(y=x, 4)
  do(f, d) %is% 0.25
})

test_that("do and %<<% on vectors respects tags", {
  do(paste, dots(sep="monkey", 1, 2, 3)) %is% "1monkey2monkey3"
})

test_that("as.dots() is idempotent on dots objects", {
  x <- 3
  l <- dots(x)
  f <- function(l) {
    x <- 4
    as.dots(l)
  }
  l <- f(l)
  x <- 5
  do(c, l) %is% 5
})

test_that("forced_dots puts literal values into dots", {
  exprs(forced_dots_(alist(1, 123L, 3, "6"))) %is% alist(1, 123L, 3, "6")
  exprs(forced_dots_(alist(a, b, "c", d))) %is% alist(quote(a), quote(b), "c", quote(d))
  exprs(forced_dots_(list(quote(...)))) %is% list(quote(quote(...)))
})

test_that("dots() et al with empty inputs", {
  f <- function(x=4, y=2) x * y
  a <- dots()
  b <- as.dots(logical(0))
  c <- list(1);
  d <- dots(2);

  do(f, a) %is% 8
  do(f, b) %is% 8
})

test_that("arg_list() makes tags by default.", {
  f <- function(a, b) {
    x <- arg_list(a, b)
    y <- arg_list(f=a, b)
    z <- arg_list(aa=a, bb=b)

    exprs(x) %is% alist(a=foo, b=bar)
    exprs(x) %is% alist(a=foo, b=bar)
  }
  f(foo, bar)
})

test_that("arg_list gets (...)",
{
  f <- function(a, b, ...) {
    arg_list(a, b, (...))
  }

  f <- compiler::cmpfun(f)

  f(foo, bar) %is% dots(a=foo, b=bar)
  f(foo, bar, baz) %is% dots(a=foo, b=bar, baz)
  f(foo, bar, baz, qux) %is% dots(a=foo, b=bar, baz, qux)
  f(foo, bar, baz, g=qux) %is% dots(a=foo, b=bar, baz, g=qux)

  f <- function(a, b, ...) {
    arg_list_(list(quote(a),
                   quote(b),
                   quote(...)),
              environment())
  }

  f(foo, bar) %is% dots(a=foo, b=bar)
  f(foo, bar, baz) %is% dots(a=foo, b=bar, baz)
  f(foo, bar, baz, qux) %is% dots(a=foo, b=bar, baz, qux)
  f(foo, bar, baz, g=qux) %is% dots(a=foo, b=bar, baz, g=qux)

  g <- function(sym, a, b, ...) {
    arg_(sym)
  }
  g("a", foo) %is% quo(foo)
  expect_error(g("...", foo, bar, baz), "\\.\\.\\.")
})

test_that("dots() on empty arguments", {
  x <- dots(, b=z)
  expect_identical(exprs(x), list(missing_value(), b=quote(z)))
  expect_identical(envs(x), list(emptyenv(), b=environment()))
  y <- x[1]
  names(y) <- "foo"
  expect_identical(exprs(y), list(foo=missing_value()))

  #check that missingness is computed and propagated correctly.
  #the following have now been fixed.
  #ref: https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=15707
  m1 <- function(x,y,z) c(missing(x), missing(y), missing(z))
  m2 <- function(...) missing_(dots(...))

  dots_other <- function(x, y, z) {
    unwrap(dots(x, y, z))
#    args(x, y, z) #makes promises set to R_MissingValue
  }

  d1 <- dots(x, , z)
  d2 <- dots_other(x, , z)

  expect_equal(c(FALSE, TRUE, FALSE), m1(one, , three))
  expect_equal(c(FALSE, TRUE, FALSE),
               m2(one, , three)) # was FALSE, FALSE, FALSE
  expect_equal(c(FALSE, TRUE, FALSE),
               (function(...) m1(...))(one, , three))
  expect_equal(c(FALSE, TRUE, FALSE),
               (function(...) m2(...))(one, , three)) # was FALSE, FALSE, FALSE
  expect_equal(c(FALSE, TRUE, FALSE),
               (function(...) (function(...) m1(...))(...))(one, , three))
  #FALSE, FALSE, FALSE but these last two are on R
  expect_equal(c(FALSE, TRUE, FALSE), do(m1, d1))
  expect_equal(c(FALSE, TRUE, FALSE), do(m1, d2)) # was FALSE, FALSE, FALSE
  expect_equal(c(FALSE, TRUE, FALSE), do(m2, d1)) # was FALSE, FALSE, FALSE
  expect_equal(c(FALSE, TRUE, FALSE), do(m2, d2)) # was FALSE, FALSE, FALSE
  expect_equal(c(FALSE, TRUE, FALSE),
               do.call(m1, alist(one, , three)))
  expect_equal(c(FALSE, TRUE, FALSE),
               do.call(m2, alist(one, , three)))
})

test_that("dots methods on empty dots", {
  x <- dots()
  missing_(x) %is% logical(0)
  names(x) %is% NULL
  expect_that(exprs(x), is_equivalent_to(list()))
  expect_equivalent(as.data.frame(x),
                    list(name=character(0), envir=list(), expr=list(), value=list()))
  x[] %is% x
  y <- dots(1, 2, 3)
  do(list, y[c()]) %is% list()
})

test_that("dots [] operator subsets without forcing promises", {
  with_setup(
    setup= {
      a <- dots(x, r=y, x+y)
      x <- 3
      y <- 4
    }, {
      do(c, a[1:2]) %is% c(3,r=4)
      x <- 4
      do(c, a[3]) %is% 8
      y <- 2
      do(c, a) %is% c(4, r=2, 6)
    }, {
      do(c, a[2:3]) %is% c(r=4, 7)
      x <- 2
      do(c, a) %is% c(2, r=4, 6)
    }, {
      do(c, a["r"]) %is% c(r=4)
    }
    )
})

test_that("[<-.... replacement operator can take values from another dotsxp", {
  #should be able to replace items of a dotslist with items from
  #another dotslist. Non-dotslists should error.
  with_setup(
    setup={
      x <- 2; y<-3;
      d <- dots(a=x, b=y, c=x+y)
    }, {
      expect_error(d[2] <- 10, "convert")
      d[2] <- quo(10)
      y <- 4
      do(c, d) %is% c(a=2, b=10, c=6)
    }, {
      d["a"] <- dots(x*y)
      x <- 5
       do(c, d) %is% c(a=15, b=3, c=8)
    })
})

test_that("dots [[]] and $ operators extract unforced promises.", {
  with_setup(
    setup={
      x <- 2; y <- 3
      d <- dots(a=x, b=y, c=x+y)
    },
    {
      d[[2]] %is% quo(y)
      x <- 1
      value(d[[1]]) %is% 1
    },
    {
      x <- 4
      d$c %is% quo(x+y)
      x <- 3
      value(d[["a"]]) %is% 3
    }
    )
})

test_that("'exprs' unpacks expressions from a dotslist", {
  d <- dots(1, x=x+1, stop("should not evaluate"))
  expect_equal(exprs(d), alist(1, x=x+1, stop("should not evaluate")))
})

test_that("dots [[<- and $<-", {
  with_setup(
      setup={
        x <- "x"; y <- 3
        d <- dots(a=x, b=y, c=x+y)
      }, {
        d[[2]] <- quo(x)
        x <- 4
        value(d[[2]]) %is% 4
      }, {
        d$b <- quo(x)
        x <- 4
        value(d) %is% list(a=4, b=4, c=x+y)
      }
  )
})

test_that("dots names method extracts tags without forcing", {
  names(dots(a, b, c=, 4, d=x+y, )) %is% c("", "", "c", "", "d", "")
  names(dots(stop("no"), a=stop("no"))) %is%  c("", "a")
  names(dots()) %is% NULL
})

test_that("dots names<- method can set tags w/o forcing", {
  with_setup(
    setup={
      x <- 2; y<-3;
      d <- dots(a=x, b=y, c=x+y)
    }, {
      names(d) <- c("foo", "bar", "baz")
      y <- 4
      do(c, d) %is% c(foo=2, bar=4, baz=6) }
    )
})

test_that("dots_", {
  e <- list2env(list(y = 12), parent=environment())
  d <- dots_(exprs=alist(y=y+1, x=1+y, z=2*y), envs = e)
  value(d) %is% list(y=13, x=13, z=24)
})

test_that("c.dots boxes quotations", {
  y <- 4
  z <- 100
  e <- dots2env(dots(x=1+y, y=1+z, `+`=`+`))
  d <- c(quo_(expr=quote(y+x), env=e))
  value(d) %is% list(106)
})

test_that("arg_, arg_", {
  f <- function(x) {
    arg(x)
  }

  g <- function(x, y, sw) arg_(sw, environment())

  expr(f(d+2)) %is% quote(d+2)
  expr(g(x+1, t+y, "y")) %is% quote(t+y)
})

test_that("quo, quo_", {
  pr <- quo(y+1)
  y <- 3
  value(pr) %is% 4

  e <- list2env(list(y=12), parent=environment())
  pr <- quo_(quote(y*3), e)
  value(pr) %is% 36
})

test_that("value of dots", {
  y <- 4
  x <- 1
  e <- dots(x=y+1, y=x+3)
  value(e) %is% list(x=5, y=4)
})

test_that("value of promise", {
  x <- 4
  e <- quo(x+5)
  x <- 8
  value(e) %is% 13
})

test_that("function_, make an empty closure", {
  r <- function_(NULL, missing_value(), emptyenv())
  expect_identical(missing_value(), body(r))
  expect_identical(formals(r), NULL)
  expect_equal(formals(r), NULL)
})

test_that("get_dots returns promise objects", {
  f <- function(...) {
    get_dots()
  }
  r <- f(1, a, 3)
  class(r[[1]]) %is% "quotation"
})

test_that("Can get missingness and forcedness of quo", {
  w <- 1
  x <- missing_value()
  delayedAssign("y", x)
  delayedAssign("z", stop("Should not force"))
  missing_(arg_list(w, x, y, z)) %is% c(w=FALSE, x=TRUE, y=TRUE, z=FALSE)
  missing_(arg_list(a=w, x)) %is% c(a=FALSE, TRUE)
  missing_(arg(w)) %is% c(FALSE)
  missing_(arg(x)) %is% c(TRUE)
  missing_(arg(y)) %is% c(TRUE)
  missing_(arg(y), unwrap=FALSE) %is% c(FALSE)
  missing_(arg(z)) %is% c(FALSE)
  is_missing(w, x, y, z) %is% c(w=FALSE, x=TRUE, y=TRUE, z=FALSE)
  missing(w) %is% FALSE
  missing(x) %is% TRUE
  missing(y) %is% TRUE
  missing(z) %is% FALSE
})

test_that("is_missing and missing_ unwraps", {
  f <- function(x, q) g(x, q)
  g <- function(y, q) h(y, q)
  h <- function(z, q) q(z)

  f(, missing) %is% TRUE
  f(, is_missing) %is% c(z=TRUE)
  f(, function(x) missing_(arg(x))) %is% TRUE

  #note that undefined != missing
  f <- function(q) g(asdlkj, q) #g(asdfghjkl, q)
  g <- function(y, q) h(y, q)
  h <- function(z, q) q(z)

  f(missing) %is% FALSE
  f(is_missing) %is% c(z=FALSE)
  f(function(x) missing_(arg(x))) %is% c(FALSE)
})
