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
#'   environment was instantiated by means `do.call`, [eval] or [do].
#'
#' @export
#'
#' @examples
caller <- function(env = caller(environment()),
                   ifnotfound = NULL) {
  ## I think we want to find the activation record that corresponds
  ## to the *earliest* invocation of our environment, and look at its
  ## sys.parent.
  ##
  ## Doing that from this side of Rinternals.h is tricky. Doubly so
  ## since sys.calls() and sys.frame() elide some of the activation
  ## records. I wrote a small package "stacktrace" which helped to
  ## figure this out.

  ## cat("getting caller of ", format(envir), "\n")

  ## print(stacktrace(), max.width=80);
  ## print(df(frames = oneline(as.list(sys.frames())),
  ##          parents = sys.parents(),
  ##          calls = oneline(as.list(sys.calls()))),
  ##       max.width=80)

  where <- which_frame(
    env,
    ifnotfound %||%
      stop("caller: environment not found on stack"))

  if (is.primitive(sys.function(where))) {
    ifnotfound %||%
      stop("caller: calling function is a primitive, which has no environment")
  }

  whichparent <- sys.parents()[where]

  if (whichparent == where) {
    # The env we are querying appears to be on the stack, but
    # sys.parents() reports it as its own parent, which, I think, is
    # what it does when the real parent is elided out by sys.parents().
    #
    # The answer I need is in sysparent, but I have no .Rinternals-level
    # way to access to sysparent. BUT, parent.frame does.
    #
    # do.call will make a frame that points to the right frame and
    # then parent.frame will get me its sysparent.
    result <- do.call(parent.frame, list(), envir=env)

    # TODO: check if the do-parent.frame trick works for other cases

    # Do I really need do.call for this? What's the way for NSE to
    # NSE to directly call with a builtin?
  } else if(whichparent == 0) {
    result <- globalenv()
  } else {
    result <- sys.frame(whichparent)
  }
  #  cat("Result: ", format(result), "\n")
  result
}

#' Making function calls, with full control of argument scope.
#'
#' The functions `do` and `do_` construct and invoke a function call.
#' In combination with [dots] and [quotation] objects they allow you to
#' control the scope of the function call and each of its arguments.
#'
#' For `do_` all arguments should be `quotation` or `dots` objects, or
#' convertible to such using `as.quo()`. They will be concatenated
#' together by `c.dots` to form the call list (a `dots` object).
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

#' Get information about calls currently being executed.
#'
#' `get_call()` takes an environment, by default the one in which it
#' was called, and determines what function call created it.  The
#' return value is a `[dots]` object; the first element of which
#' represents the calling environment and the name of the function as
#' it appeared there. The rest of the elements represent the function
#' args.
#'
#' The output of [get_call()] can be passed to [do(x)] in order to
#' replicate a call.
#'
#' `get_call` is meant to replace `[match.call()]` and `[sys.call()]`;
#' its advantage is that it captures the environments attached to
#' arguments in addition to their written form.
#'
#' @return a dots object, the first element of which represents the
#'   call.
#' @seealso do dots caller
#' @export
#' @param env An environment belonging to a currently executing
#'   function call.
#' @param ifnotfound If the call is not found, an error is raised, or
#'   optionally this is returned.
get_call <- function(env = caller(environment()),
                     ifnotfound = stop("caller: environment not found on stack")) {
  frame <- which_frame(env, ifnotfound)

  rho <- caller(env)
  call <- sys.call(frame)
  head <- call[[1]]
  fn <- sys.function(frame)
  argnames <- names(formals(fn))
  c.dots(quo_(head, rho),
         env2dots(env, argnames));
}

#' `get_function(env)` returns the function object associated with a
#' currently executing call (identified by environment).
#' @rdname get_call
#' @export
get_function <- function(env = caller(environment()),
                         ifnotfound = NULL) {
  sys.function(which_frame(env, ifnotfound))
}

which_frame <- function(env, ifnotfound) {
  frames <- sys.frames()
  where <- which(vapply(frames, identical, FALSE, env))
  if (length(where) == 0) {
    ifnotfound %||% stop("caller: environment not found on stack")
  } else {
    where[1]
  }
}
