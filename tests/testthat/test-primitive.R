
## # So here's a list of base SPECIALSXPs:
## keep.where <- function(x, pred) x[mapply(pred, x)]
## specials <- (baseenv() |> as.list() |> keep.where(is.primitive)
##   |> keep.where(function(x) capture.output(.Internal(inspect(x)))[1]
##                 |> grepl(pattern="SPECIAL"))
##   |> names() |> sort())
`%is%` <- expect_equal

check_calls <- function(x) {capture.output(print(sys.calls())); x}

test_that("primitive functions that don't handle `...`", {
  # watchdog test: demonstrate primitive functions that don't expand `...`

  dotwrap <- function(fn) function(...) fn(...)
  expect_dotfail <- function(call, pattern=NA) {
    call_ <- arg(call)
    native_result <- force(call) #what the call would no "natively"
    expr(call_)[[1]] <- base::call("dotwrap", expr(call_)[[1]])
    dots_result <- function() NULL
    expect_error({
      dots_result <- value(call_)
      expect_identical(native_result, dots_result)
    }, pattern)
    if (identical(native_result, dots_result)) stop("didn't fail")
    #now check that do_ does something better?
  }

  # i.e. expect_error(dotwrap(`::`)(async, do))
  expect_dotfail(nseval::do, "1 argument")
  expect_dotfail(nseval:::do__, "1 argument")
  expect_dotfail({cat("hello"); cat("world")}, "...")

  setClass("track", slots = c(x="numeric", y="numeric"))
  myTrack <- new("track", x = -4:4, y = exp(-4:4))
  expect_dotfail(myTrack@x, "1 argument")

  expect_dotfail(TRUE && FALSE, "requires 2")
  expect_dotfail(x <- 5, "number of arguments")
  expect_dotfail(x <<- 5, "number of arguments")
  expect_dotfail(`=`(y, 5), "number of arguments")
  expect_dotfail(FALSE || TRUE, "requires 2")
  expect_dotfail(x ~ y, "not identical")

  x <- list(a=1, b=2)
  expect_dotfail(x$a, "requires 2")
  expect_dotfail(call("hello", "world"), "\\.\\.\\.")
  expect_dotfail(expression(hello(world)), "not identical")

  x <- 0
  expect_dotfail(for (i in 1:10) {x <- x + i}, "1 argument")
  expect_dotfail(forceAndCall(2, dots, 2+2, 3+3, 4+4, 5+5), "\\.\\.\\.")

  expect_dotfail(function(x)y, "number of arguments")
  expect_dotfail(if(TRUE) 1 else 2, "\\.\\.\\.")
  y <- missing_value()
  expect_dotfail(missing(y), "not identical")
  expect_dotfail(on.exit("hello"), "\\.\\.\\.")
  expect_dotfail(quote(x), "not identical")
  expect_dotfail(repeat {cat(1); break}, "\\.\\.\\.")
  (function(x) expect_dotfail(substitute(hello(x)), "not identical"))(1+1)
  expect_dotfail(switch(3, 1, 2, 3, 4, 5), "\\.\\.\\.")
  expect_dotfail(switch("b", a=1, b=2, c=3, d=4, e=5), "\\.\\.\\.")

  # UseMethod fails but on the first UseMethod call. Because UseMethod
  # expects to be called directly from the function's stack frame; it
  # needs to be called in the lowest stack frame, can't wrap anything
  # around it.
  ## test <- function(x) identity(UseMethod("test"))
  ## test <- function(x) expect_dotfail(UseMethod("test"), "inappropriate")
  ## test.list <- function(x) "a list!"
  ## test(list(1, 2))
  expect_dotfail(while(TRUE) break, "requires 2")
})

#seems like these are the "trouble" primitives.
trouble_primitives <-
  c("::", ":::", "{", "@", "&&", "<-", "<<-", "=", "||", "~",
    "$", "expression", "for", "forceAndCall", "function",
    "if", "missing", "on.exit", "quote", "repeat", "substitute", "switch",
    "UseMethod", "while")

#Calling strategies for each:
strategies <- list(
  `::` ="literal",
  `:::`="literal",
  `{`="promsxps",
  `@`="slot", #promise head, call in 1st arg's env, literal rest
  `&&` = "promsxps",
  `<-` = "store", #promise head, call in 1st arg's env, promsxp rest
  `<<-` = "store",
  `~` = "literal",
  `$` = "slot",
  `expression` = "literal",
  `for` = "literal", #you can promsxp the range I guess
  `forceAndCall` = "forceAndCall", #first arg is promsxp, rest are quoted (but you can pass dots.... in "rest",
  `function` = "literal",
  `if`="promsxps",
  `missing`="store",
  `on.exit`="literal",
  `quote`="literal",
  `repeat`="literal",
  `substitute`="promsxps",
  `switch`="promsxps",
  `UseMethod`="promsxps", #probably still won't work though
  `while`="literal"
)

#Some checks on the above classifications
test_that("when can we use promsxps?", {
  # test this right now with a locked environment
  e <- new.env()
  x <- "outie"
  e$x <- "innie"
  lockEnvironment(e)
  expect_error(do_(quo(`::`, e), quo(nseval, e), quo(do)), "name")
  expect_error(do_(quo(`:::`, e), quo(nseval, e), quo(do)), "name")

  #{ can promsxp
  fx <- function(x) {
    do_(quo(`{`, e),
        quo(x %is% "innie", e),
        quo(x %is% "argie"),
        quo(check_calls(TRUE)))
  } #promsxp works and does not show in syscalls!
  fx("argie") %is% TRUE

  setClass("track", slots = c(x="numeric", y="numeric"))
  myTrack <- new("track", x = -4:4, y = exp(-4:4))
  expect_error(do_(dots_(alist(`@`, myTrack), e), dots(x)), "slot")

  #&& can use promsxps
  fg <- function() {
    do_(dots_(alist(`&&`, check_calls(TRUE)), e),
        dots(check_calls({x <- 2; TRUE})))
  }
  fg()

  #forceAndCall can promsxp the first arg as promise and unpack ... after the second?
  e <- new.env()
  e$x <- 1
  x <- 2
  set_dots(e, c(dots(1+1, 2+2), quo(3+3, e), quo(4+4)))
  expr <- quote(forceAndCall(nargs, dots, ...))
  expr[[2]] <- .Call("_quotation_to_promsxp", quo(x+x, e))
  eval(expr, e)

  #"if" can use promsxps, just not ...
  lockEnvironment(e)
  do_(quo(`if`, e), quo(x==1, e), quo(x), quo(FALSE)) %is% 2

  #"substitute" works perfectly with promsxps
  e1 <- (function(l=a[[b]], e=e2) environment())()
  e2 <- (function(l=x[[y]], e=e1) environment())()
  expr <- quote(substitute(x <- y, e))
  expr[[2]] <- .Call("_quotation_to_promsxp", quo(l, e1))
  expr[[3]] <- .Call("_quotation_to_promsxp", quo(e, e2))
  eval(expr) %is% quote(a[[b]])

  #"switch" can promsxp fine
  e1 <- (function(x=1, y="e1y", z="e1z") environment())()
  e2 <- (function(x=2, y="e2y", z="e2z") environment())()
  expr <- quote(switch(x, y, z))
  expr[[2]] <- .Call("_quotation_to_promsxp", quo(x, e1))
  expr[[3]] <- .Call("_quotation_to_promsxp", quo(check_calls(y), e2))
  expr[[4]] <- .Call("_quotation_to_promsxp", quo(z, e1))
  eval(expr) %is% "e2y"

  #"for" can promsxp the range arg
  e1 <- (function(x=2, y="e1y", z="e1z") environment())()
  e2 <- (function(x=3, y="e2y", z="e2z") environment())()
  expr <- quote(for(i in 1:check_calls(x)) {x <- check_calls(x) + 1})
  expr[[3]] <- .Call("_quotation_to_promsxp", quo_(expr[[3]], e2))
  eval(expr, e1)
  e1$x %is% 5

  #can you promsxp the head of a call?"
  x <- 1
  expr <- quote(x <- check_calls(TRUE))
  e <- new.env()
  expr[[1]] <- .Call("_quotation_to_promsxp", quo(`<-`))
  eval(expr, e)
  expect_equal(x, 1); expect_equal(e$x, TRUE)
  #looks like yes, and the call is in env e...
})

