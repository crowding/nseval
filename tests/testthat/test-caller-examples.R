#Worked examples for "caller" with interpreter-level stack traces.

library(testthat)
`%is%` <- expect_equal
`%throws%` <- expect_error
context("caller examples")

#debug(caller)
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
        caller()$where %is% "f"
      }
      g()
    }
    f()
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



# Example 3

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
  e()()$where %throws% "e"
})

#    callflag evaldepth                       promargs                        callfun                  sysparent                           call                     cloenv
# 1         0         0                           NULL                           NULL        <environment: base>                           NULL        <environment: base>
# 2        12         1 args(~/fexpr/inst/tests/te.... function (file, local = FA.... <environment: R_GlobalEnv> source("~/fexpr/inst/tests.... <environment: 0x107c86a98>
# 3        12         2 args(<environment: 0x107c8....               function (x) ... <environment: 0x107c86a98>   withVisible(eval(ei, envir)) <environment: 0x10900a4d0>
# 4        12         5 args(ei := expression(test.... function (expr, envir = pa.... <environment: 0x107c86a98>                eval(ei, envir) <environment: 0x10900a310>
# 5        12         6 args(expression(test_that(....             .Primitive("eval") <environment: 0x10900a310>      eval(expr, envir, enclos) <environment: R_GlobalEnv>
# 6        12         7 args(caller from a lazy ar....      function (desc, code) ... <environment: R_GlobalEnv> test_that("caller from a l.... <environment: 0x109009e40>
# 7        12         9 args(desc := caller from a.... function (description, cod.... <environment: 0x109009e40> test_code(desc, substitute.... <environment: 0x10900ab80>
# 8        12        11 args(<environment: 0x10900.... function (expr, ..., final.... <environment: 0x10900ab80> tryCatch(withCallingHandle.... <environment: 0x10900dc60>
# 9        12        12 args(<environment: 0x10900.... function (expr, names, par.... <environment: 0x10900dc60> tryCatchList(expr, classes.... <environment: 0x10900e460>
# 10       12        13 args(<environment: 0x10900.... function (expr, name, pare.... <environment: 0x10900e460> tryCatchOne(tryCatchList(e.... <environment: 0x10900e118>
# 11       12        14 args(<environment: 0x10900.... function (expr, name, pare.... <environment: 0x10900e118> doTryCatch(return(expr), n.... <environment: 0x10900ed08>
# 12       12        17 args(<environment: 0x10900.... function (expr, names, par.... <environment: 0x10900e460> tryCatchList(expr, names[-.... <environment: 0x10900e6e8>
# 13       12        18 args(<environment: 0x10900.... function (expr, name, pare.... <environment: 0x10900e6e8> tryCatchOne(expr, names, p.... <environment: 0x10900f2d8>
# 14       12        19 args(<environment: 0x10900.... function (expr, name, pare.... <environment: 0x10900f2d8> doTryCatch(return(expr), n.... <environment: 0x10900ef20>
# 15       12        25 args(<environment: 0x10900....       function (expr, ...) ... <environment: 0x10900ab80> withCallingHandlers(eval(c.... <environment: 0x10900f8a8>
# 16       12        27 args(code := {..., new_tes.... function (expr, envir = pa.... <environment: 0x10900ab80> eval(code, new_test_enviro.... <environment: 0x109010988>
# 17       12        28 args({... := {..., <enviro....             .Primitive("eval") <environment: 0x109010988>      eval(expr, envir, enclos) <environment: 0x10900a720>
# 18       12        30 args(<environment: 0x10900.... function (object, expected.... <environment: 0x10900a720>                e()() %is*% "e" <environment: 0x109011310>
# 19       12        32 args(<environment: 0x10901.... function (object, conditio.... <environment: 0x109011310> expect_that(object, throws.... <environment: 0x1037a22d8>
# 20       12        35 args(<environment: 0x1037a....            function (expr) ... <environment: 0x1037a22d8>              condition(object) <environment: 0x10707a838>
# 21       12        38 args(<environment: 0x10707.... function (expr, silent = F.... <environment: 0x10707a838>         try(force(expr), TRUE) <environment: 0x10707a640>
# 22       12        39 args(<environment: 0x10707.... function (expr, ..., final.... <environment: 0x10707a640> tryCatch(expr, error = fun.... <environment: 0x10707be08>
# 23       12        40 args(<environment: 0x10707.... function (expr, names, par.... <environment: 0x10707be08> tryCatchList(expr, classes.... <environment: 0x10707ed78>
# 24       12        41 args(<environment: 0x10707.... function (expr, name, pare.... <environment: 0x10707ed78> tryCatchOne(expr, names, p.... <environment: 0x10707e6e8>
# 25       12        42 args(<environment: 0x10707.... function (expr, name, pare.... <environment: 0x10707e6e8> doTryCatch(return(expr), n.... <environment: 0x103798748>
# 26       12        48 args(<environment: 0x10707....               function (x) ... <environment: 0x10707a838>                    force(expr) <environment: 0x103798390>
# 27       12        53                           NULL               function (f) ... <environment: 0x10900a720>                          e()() <environment: 0x103798e30> # the function called; is "g"
# 28       12        55                           NULL function (envir = caller(e.... <environment: 0x103799178>                       caller() <environment: 0x103798d88> # target env for some reason
# 29       12        58                           NULL                function () ... <environment: 0x103798d88>                   stacktrace() <environment: 0x101b586d8>

# envir: <environment: 0x103798d88>

#       Frames:                      parent                              call  
# [[1]] <environment: 0x107c86a98>   0       source("~/fexpr/inst/tests....
# [[2]] <environment: 0x10900a4d0>   1         withVisible(eval(ei, envir))
# [[3]] <environment: 0x10900a310>   1                      eval(ei, envir)
# [[4]] <environment: R_GlobalEnv>   3            eval(expr, envir, enclos)
# [[5]] <environment: 0x109009e40>   0       test_that("caller from a l....
# [[6]] <environment: 0x10900ab80>   5       test_code(desc, substitute....
# [[7]] <environment: 0x10900dc60>   6       tryCatch(withCallingHandle....
# [[8]] <environment: 0x10900e460>   7       tryCatchList(expr, classes....
# [[9]] <environment: 0x10900e118>   8       tryCatchOne(tryCatchList(e....
# [[10]] <environment: 0x10900ed08>  9       doTryCatch(return(expr), n....
# [[11]] <environment: 0x10900e6e8>  8       tryCatchList(expr, names[-....
# [[12]] <environment: 0x10900f2d8>  11      tryCatchOne(expr, names, p....
# [[13]] <environment: 0x10900ef20>  12      doTryCatch(return(expr), n....
# [[14]] <environment: 0x10900f8a8>  6       withCallingHandlers(eval(c....
# [[15]] <environment: 0x109010988>  6       eval(code, new_test_enviro....
# [[16]] <environment: 0x10900a720>  15           eval(expr, envir, enclos)
# [[17]] <environment: 0x109011310>  16                     e()() %is*% "e"
# [[18]] <environment: 0x1037a22d8>  17      expect_that(object, throws....
# [[19]] <environment: 0x10707a838>  18                   condition(object)
# [[20]] <environment: 0x10707a640>  19              try(force(expr), TRUE)
# [[21]] <environment: 0x10707be08>  20      tryCatch(expr, error = fun....
# [[22]] <environment: 0x10707ed78>  21      tryCatchList(expr, classes....
# [[23]] <environment: 0x10707e6e8>  22      tryCatchOne(expr, names, p....
# [[24]] <environment: 0x103798748>  23      doTryCatch(return(expr), n....
# [[25]] <environment: 0x103798390>  19                         force(expr)
# [[26]] <environment: 0x103798e30>  16                               e()()
# [[27]] <environment: 0x103798d88>  27                            caller()  *target env* note it is its own sysparent!

# where=27

# Possible lesson: if we are our own sysparent, reject. 
