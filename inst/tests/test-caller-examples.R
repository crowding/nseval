

#Worked examples with stack traces.
library(testthat)
`%is%` <- expect_equal
`%throws%` <- expect_error
context("caller examples")
#undebug(caller)
#1

test_that("Example 1", {
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
        caller()$where %is% ""
      }
    }
  }
  e()
})

#    callflag evaldepth                       promargs                        callfun                  sysparent                           call                     cloenv
# 1         0         0                           NULL                           NULL        <environment: base>                           NULL        <environment: base>
# 2        12         1 args(~/fexpr/Untitled.R, e.... function (file, local = FA.... <environment: R_GlobalEnv> source("~/fexpr/Untitled.R.... <environment: 0x10d172db8>
# 3        12         2 args(<environment: 0x10d17....               function (x) ... <environment: 0x10d172db8>   withVisible(eval(ei, envir)) <environment: 0x10d1c79c8>
# 4        12         5 args(ei := expression(test.... function (expr, envir = pa.... <environment: 0x10d172db8>                eval(ei, envir) <environment: 0x10d1c7808>
# 5        12         6 args(expression(test_that(....             .Primitive("eval") <environment: 0x10d1c7808>      eval(expr, envir, enclos) <environment: R_GlobalEnv>
# 6        12         7 args(caller from eval and ....      function (desc, code) ... <environment: R_GlobalEnv> test_that("caller from eva.... <environment: 0x10d1c84a0>
# 7        12         9 args(desc := caller from e.... function (description, cod.... <environment: 0x10d1c84a0> test_code(desc, substitute.... <environment: 0x10d1c8238>
# 8        12        11 args(<environment: 0x10d1c.... function (expr, ..., final.... <environment: 0x10d1c8238> tryCatch(withCallingHandle.... <environment: 0x10d1ca548>
# 9        12        12 args(<environment: 0x10d1c.... function (expr, names, par.... <environment: 0x10d1ca548> tryCatchList(expr, classes.... <environment: 0x10d1cadb8>
# 10       12        13 args(<environment: 0x10d1c.... function (expr, name, pare.... <environment: 0x10d1cadb8> tryCatchOne(tryCatchList(e.... <environment: 0x10d1caa70>
# 11       12        14 args(<environment: 0x10d1c.... function (expr, name, pare.... <environment: 0x10d1caa70> doTryCatch(return(expr), n.... <environment: 0x10d1cb6d0>
# 12       12        17 args(<environment: 0x10d1c.... function (expr, names, par.... <environment: 0x10d1cadb8> tryCatchList(expr, names[-.... <environment: 0x10d1cb120>
# 13       12        18 args(<environment: 0x10d1c.... function (expr, name, pare.... <environment: 0x10d1cb120> tryCatchOne(expr, names, p.... <environment: 0x10d1cbd10>
# 14       12        19 args(<environment: 0x10d1c.... function (expr, name, pare.... <environment: 0x10d1cbd10> doTryCatch(return(expr), n.... <environment: 0x10d1cb9c8>
# 15       12        25 args(<environment: 0x10d1c....       function (expr, ...) ... <environment: 0x10d1c8238> withCallingHandlers(eval(c.... <environment: 0x10d1cc3c0>
# 16       12        27 args(code := {..., new_tes.... function (expr, envir = pa.... <environment: 0x10d1c8238> eval(code, new_test_enviro.... <environment: 0x10d1cccd8>
# 17       12        28 args({... := {..., <enviro....             .Primitive("eval") <environment: 0x10d1cccd8>      eval(expr, envir, enclos) <environment: 0x10d1c8df0>
# 18       12        30                           NULL                function () ... <environment: 0x10d1c8df0>                            e() <environment: 0x10d1cc8b0>
# 19       12        32                           NULL                function () ... <environment: 0x10d1cc8b0>                            f() <environment: 0x10d1cd5f0>
# 20       12        34                           NULL                function () ... <environment: 0x10d1cd5f0>                            g() <environment: 0x10d1cd388>     * the goal! Why: is sysparent of the subject frame
# 21       12        36 args(<environment: 0x10d1c.... function (object, expected.... <environment: 0x10d1cd388>        caller()$where %is% "f" <environment: 0x10d1cdf40>     |        An ordinary call.
# 22       12        38 args(<environment: 0x10d1c.... function (object, conditio.... <environment: 0x10d1cdf40> expect_that(object, equals.... <environment: 0x10d1d4a38>     |        An ordinary call.
# 23       12        41 args(<environment: 0x10d1d....          function (actual) ... <environment: 0x10d1d4a38>              condition(object) <environment: 0x10d1d6ca0>     |    *
# 24       12        44 args(<environment: 0x10d1d....       function (x, y, ...) ... <environment: 0x10d1d6ca0> compare(actual, expected, ...) <environment: 0x10d1d6aa8>     |    ^ an ordinary call (to an S3 generic)
# 25       12        52                           NULL function (envir = caller(e.... <environment: 0x10d1cd388>                       caller() <environment: 0x10d1d6920>   *-^ * * the target frame. Why? Is only activation record with this cloenv. Is also a promise eval
# 26       12        53 args(<environment: 0x10d1d.... function (x, arr.ind = FAL.... <environment: 0x10d1d6920> which(vapply(st$cloenv, id.... <environment: 0x10d1e1698>   |   | ^ an ordinary call. Why? Is
# 27       12        55 args(st$cloenv := <environ.... function (X, FUN, FUN.VALU.... <environment: 0x10d1d6920> vapply(st$cloenv, identica.... <environment: 0x10d1e1318>   | *-^   a delayed promise eval. Why? sysparent is not prev frame's cloenv. 
# 28       12        56 args(X[[i]] := <environmen.... function (x, y, num.eq = T.... <environment: 0x10d1e1318>               FUN(X[[i]], ...) <environment: 0x10d1e3510>   | ^     A normal call. Sysparent is prev frame's cloenv.
# 29       12        60 args(environment() := <env.... function (envir = caller(e.... <environment: 0x10d1d6920>          caller(environment()) <environment: 0x10d1e32a8> *-^       The call to "caller" in question.
#                                                                                                                                                                          |           Also a delayed promise, because sysparent is not last cloenv. envir is given as 0x10d1d6920.
# 30       12        61                           NULL                function () ... <environment: 0x10d1e32a8>       stacktrace::stacktrace() <environment: 0x10d1e4e98> ^
# 
# Proposed algo: March up sysperent to find highest activation record that has the necessary sysparent. Then return that record's sysparent.


# debug(caller)
test_that("foo", {
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
    eval(quote(caller()), y)$where %throws% "e"
  }
  h()
})


# worked example:
#
#    callflag evaldepth                       promargs                        callfun                  sysparent                           call                     cloenv
# 1         0         0                           NULL                           NULL        <environment: base>                           NULL        <environment: base>
# 2        12         1 args(~/fexpr/inst/tests/te.... function (file, local = FA.... <environment: R_GlobalEnv> source("~/fexpr/inst/tests.... <environment: 0x102cca4d8>
# 3        12         2 args(<environment: 0x102cc....               function (x) ... <environment: 0x102cca4d8>   withVisible(eval(ei, envir)) <environment: 0x102b77d10>
# 4        12         5 args(ei := expression(loca.... function (expr, envir = pa.... <environment: 0x102cca4d8>                eval(ei, envir) <environment: 0x102b77f08>
# 5        12         6 args(expression(local({.......             .Primitive("eval") <environment: 0x102b77f08>      eval(expr, envir, enclos) <environment: R_GlobalEnv>
# 6        12         7 args(<environment: R_Globa.... function (expr, envir = ne.... <environment: R_GlobalEnv>                     local({... <environment: 0x102b76a38>
# 7        12         8 args(substitute(eval(quote....     function (expr, n = 1) ... <environment: 0x102b76a38> eval.parent(substitute(eva.... <environment: 0x102b76bf8>
# 8        12         9 args(expr := eval(quote({..... function (expr, envir = pa.... <environment: 0x102b76bf8>                  eval(expr, p) <environment: 0x102b76078>
# 9        12        10 args(eval(quote({... := ev....             .Primitive("eval") <environment: 0x102b76078>      eval(expr, envir, enclos) <environment: R_GlobalEnv>
# 10       12        11 args(quote({... := {..., n.... function (expr, envir = pa.... <environment: R_GlobalEnv>                eval(quote({... <environment: 0x102b75840>
# 11       12        12 args({... := {..., <enviro....             .Primitive("eval") <environment: 0x102b75840>      eval(expr, envir, enclos) <environment: 0x102b75cd8>
# 12       12        14                           NULL                function () ... <environment: 0x102b75cd8>                            h() <environment: 0x102b74b18>
# 13       12        16 args(<environment: 0x102b7.... function (object, expected.... <environment: 0x102b74b18> eval(quote(caller()), y)$w.... <environment: 0x102b74e98>
# 14       12        18 args(<environment: 0x102b7.... function (object, conditio.... <environment: 0x102b74e98> expect_that(object, throws.... <environment: 0x102b66d80>
# 15       12        21 args(<environment: 0x102b6....            function (expr) ... <environment: 0x102b66d80>              condition(object) <environment: 0x102b64f08>
# 16       12        24 args(<environment: 0x102b6.... function (expr, silent = F.... <environment: 0x102b64f08>         try(force(expr), TRUE) <environment: 0x102b641c8>
# 17       12        25 args(<environment: 0x102b6.... function (expr, ..., final.... <environment: 0x102b641c8> tryCatch(expr, error = fun.... <environment: 0x102b643c0>
# 18       12        26 args(<environment: 0x102b6.... function (expr, names, par.... <environment: 0x102b643c0> tryCatchList(expr, classes.... <environment: 0x102b63b50>
# 19       12        27 args(<environment: 0x102b6.... function (expr, name, pare.... <environment: 0x102b63b50> tryCatchOne(expr, names, p.... <environment: 0x102b63f08>
# 20       12        28 args(<environment: 0x102b6.... function (expr, name, pare.... <environment: 0x102b63f08> doTryCatch(return(expr), n.... <environment: 0x102b63318>
# 21       12        34 args(<environment: 0x102b6....               function (x) ... <environment: 0x102b64f08>                    force(expr) <environment: 0x102b63708>
# 22       12        40 args(quote(caller()) := ca.... function (expr, envir = pa.... <environment: 0x102b74b18>       eval(quote(caller()), y) <environment: 0x102b62a70>
# 23       12        41 args(caller() := caller(),....             .Primitive("eval") <environment: 0x102b62a70>      eval(expr, envir, enclos) <environment: 0x102b75350>  # the only frame with the target cloenv is a primitive call to eval.
# 24       12        42                           NULL function (envir = caller(e.... <environment: 0x102b75350>                       caller() <environment: 0x102b62c68>
# 25       12         3                           NULL                function () ... <environment: 0x102b62c68>       stacktrace::stacktrace() <environment: 0x1031d43b8>

# envir = <environment: 0x102b75350>
# st$promargs[[22]] = args(quote(caller()) := caller(), y := <environment: 0x102b75350>)

# Learning: If the call's callfun is a primitive, it is probably not a "real" call, at least its cloenv is not to be trusted. 
