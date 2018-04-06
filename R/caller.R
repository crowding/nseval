#' Get the calling environment of a given environment.
#'
#' @param env The environment to inspect. By default, the
#'   environment from which \code{caller} itself was called. (That is,
#'   `caller()` should always return the same value as
#'   `caller(environment())`.)
#'
#' @return The environment in which the function call that created
#'   `env` was executed. If that environment cannot be reliably
#'   determined, an error is raised.
#'
#'   For example, in the code:
#'
#'       X <- environment()
#'       F <- function() {
#'         Y <- environment()
#'         caller(Y)
#'       }
#'       F()
#'
#'   the environment called "Y" was created by calling F(), and that
#'   call occurs in the environment called "X". In this case X is the
#'   calling environment of Y, so `F()` returns the same environment
#'   as `X()`.
#'
#'   `caller` is intended as a replacement for [parent.frame], which
#'   returns the next environment up the calling stack -- which is
#'   sometimes the same value, but differs in some cases (e.g. when
#'   lazy evaluation re-activates an environment.) So `parent.frame()`
#'   can return different things depending on the order in which
#'   arguments are evaluated.
#'
#'   In addition, `caller` tries to do the right thing when the
#'   environment was created by `do.call`, [eval] or [do].
#'
#' @export
#'
#' @examples
#' # For further examples please see the test cases
#' # located under inst/tests/test-caller.R
caller <- function(envir=caller(environment())) {
  # assign("trace",
  #        list(trace=stacktrace(), frames=sys.frames(),
  #             parents = sys.parents(), calls = sys.calls(), envir = envir),
  #       envir = globalenv())

  frames <- sys.frames()
  where <- which(vapply(frames, identical, FALSE, envir))

  if (length(where) == 0) {
    stop("caller: environment not found on stack")
  }

  if (is.primitive(sys.function(where[1]))) {
    stop("caller: calling function is a primitive, which has no environment")
  }

  parents <- sys.parents()
  whichparent <- parents[where[1]]

  if (whichparent == where[1]) {
    stop("caller: caller is no longer on stack")
  }

  if(whichparent == 0) {
    globalenv()
  } else {
    frames[[whichparent]]
  }
}


#' Wrap a function so that it will see a particular caller or parent.frame.
#'
#' @param f The function to call. A symbol or expression may be given, which must be
#'   a symbol visible from \code{e}.
#'
#' @param envir The environment to issue the call from. If
#'   \code{\link{caller}()} is called within \code{f}, it will return
#'   \code{envir}.
#'
#' @return A function wrapper. Calling it will call \code{f}, but from
#'   within \code{f} a call to \code{parent.frame()} or \code{caller}
#'   will return \code{envir}.
#'
#' Use \code{with_caller} to deal with or wrap functions that use
#' their \code{caller()} or \code{parent.frame()}. (Try not to write
#' such functions though!)
#'
#' \code{with_caller} is intended to be safer than
#' \code{\link{do.call}} for this purpose.  A function wrapped by
#' with_caller passes its arguments normally, while \code{do.call}
#' causes the function arguments to also be interpreted in the target
#' environment.
#'
#' To reproduce the effects of
#' \code{do.call(f, alist(foo, bar), envir=e)}, you could write
#' \code{with_caller(f, e) \%()\% dots(alist(foo, bar), e)}.
#'
#' Note that there is independent control of the caller of the function and
#' the context of the arguments.
#' @export
#' @useDynLib nse _make_call
with_caller <- function(f, env=arg_env(f)) {
  head <- quo_(f, envir)
  function(...) {
    do_(head, dots(...))
  }
}

#' Making function calls, with full control of call construction.
#'
#' The functions `do` and `do_` construct and invoke a function call.
#' In combination with [dots] and [quotation] objects they allow you to
#' control the scope of the function call and each of its arguments.
#'
#' For `do_` all arguments should be `quotation` or `dots` objects, or
#' convertible to such using `as.quo()`. They will be concatenated
#' together by `c.dots` to form the call list (a "dots" object).
#'
#' For `do` the first argument is captured unevaluated, but the
#' rest of the arguments are processed the same as do_.
#'
#' The first element of the call list represents the function, and it
#' should [evaluate](value) to a function object. The rest of the call
#' list is used as that function's arguments.
#'
#' When a quotation is given as the function, the call is evaluated
#' from the environment given in that quotation. This means that calls
#' to [caller()] (or `parent.frame()`) from within that function
#' should get back to that environment. Meanwhile, using `sys.call` or
#' `match.call` from that function should return the original expression.
#'
#' Note that primitive functions (e.g. [`<-`], [`for`]) may require
#' that certain arguments come from the same environment as the
#' calling environment.
#'
#' `do` is intended to be a replacement for base function [do.call].
#'
#' @seealso get_call do.call match.call
#'
#' @param ... All arguments are concatenated using `c.dots()`. The
#'   first element of the resulting list is taken as a function to
#'   call, the rest as its arguments.
#'
#' @return The return value of the call.
#' @export
do <- function(...) {
  d <- dots(...)
  d[[1]] <- as.quo.literal(arg(..1)) #unwrap and then insulate from forcing
  d <- do_(quo(c.dots), d) #force dots and concatenate
  do__(d)
}

#' @rdname do
#' @useDynLib nse _do
#' @export
do_ <- function(...) {
  d <- c.dots(...)
  do__(d)
}

do__ <- function(d) {
  .Call(`_do`, d)
}

#' Obtain the call and arguments associated with an environment.
#'
#' The output of [get_call()] can be passed to [do] in order to
#' replicate a call;
#'
#' `get_call` is meant to replace `[match.call()]` and `[sys.call()]`
#'
#' @return a dots object; the first element of which represents the call
#' @seealso do dots
get_call <- function(env = caller()) {
  stop("unimplemented")
}
