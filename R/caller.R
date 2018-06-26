#' Find the caller of a given environment.
#'
#' Given an environment that is currently on the stack, `caller`
#' determines the calling environment, that is
#'
#' For example, in the code:
#'
#'       X <- environment()
#'       F <- function() {
#'         Y <- environment()
#'         caller(Y)
#'       }
#'       F()
#'
#' the environment called "Y" was created by calling F(), and that
#' call occurs in the environment called "X". In this case X is the
#' calling environment of Y, so `F()` returns the same environment
#' as `X()`.
#'
#' `caller` is intended as a replacement for [parent.frame], which
#' returns the next environment up the calling stack -- which is
#' sometimes the same value, but differs in some cases such as when
#' lazy evaluation re-activates an environment. `parent.frame()` can
#' return different things depending on the order in which arguments
#' are evaluated, and without warning. `caller` will by default throw
#' an error if the caller cannot be determined.
#'
#' In addition, `caller` tries to do the right thing when the
#' environment was instantiated by means of `do.call`, [eval] or
#' [do] rather than an ordinary function call.
#'
#' @param env The environment whose caller to find. The default is
#'   `caller`'s caller; that is, `caller()` should return the the same
#'   value as `caller(environment())`.)
#' @param ifnotfound What to return in case the caller cannot be
#'   determined. By default an error is raised.
#' @return The environment which called `env` into being. If that
#'   environment cannot be determined, `ifnotfound` is returned.
#'
#' @export
#'
#' @examples
#' E <- environment()
#' F <- function() {
#'  Y <- environment()
#'  caller(Y)
#' }
#' identical(F(), E) ## TRUE
caller <- function(env = caller(environment()),
                   ifnotfound = stop("caller: environment not found on stack")) {
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

  where <- which_frame(env, ifnotfound)

  if (is.primitive(sys.function(where))) {
    if (is_default(ifnotfound)) {
      stop("caller: calling function is a primitive, which has no environment")
    } else ifnotfound
  }

  whichparent <- sys.parents()[where]

  if (whichparent == where) {
    # The env we are querying appears to be on the stack, but
    # sys.parents() reports it as its own parent, which, I think, is
    # what it does when the real parent is elided out for some reason.
    #
    # The answer I need is in sysparent, but I have no .Rinternals-level
    # way to access sysparent (beyond sys.parent() which mangles it.)
    #
    # BUT, existing parent.frame uses sysparent.
    #
    # do.call will make a stack frame that has the right sysparent, which
    # parent.frame will return.
    result <- do.call(parent.frame, list(), envir=env)

    # Do I really need do.call for this? What's the way for
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
#' In combination with [dots] and [quotation] objects they allow you
#' to control the scope of the function call and each of its arguments
#' independently.
#'
#' For `do_` all arguments should be `quotation` or `dots` objects, or
#' convertible to such using `as.quo()`. They will be concatenated
#' together by [c.dots] to form the call list (a `dots` object).
#' For `do` the first argument is quoted literally, but the
#' rest of the arguments are evaluated the same way as do_.
#'
#' The first element of the call list represents the function, and it
#' should evaluate to a function object. The rest of the call list is
#' used as that function's arguments.
#'
#' When a quotation is used as the first element, the call is evaluated
#' from the environment given in that quotation. This means that calls
#' to [caller()] (or `parent.frame()`) from within that function
#' should return that environment.
#'
#' `do` is intended to be a replacement for base function [do.call].
#'
#' @note [primitive](is.primitive) functions (e.g. [`<-`], [`for`])
#'   may require that they are called from the same environment as
#'   their args.
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
  d[[1]] <- forced_quo(arg(..1)) #unwrap and then insulate from forcing
  d <- do_(quo(c.dots), d)       #force dots and concatenate
  do__(d)
}

#' @rdname do
#' @useDynLib nseval _do
#' @export
do_ <- function(...) {
  d <- c.dots(...)
  do__(d)
}

do__ <- function(d) {
  .Call("_do", d)
}

#' Get information about currently executing calls.
#'
#' `get_call(env)`, given an environment, returns the function call
#' and its arguments, as a [dots] object which can be passed to [`do()`]
#' in order to replicate a call.
#'
#' `get_call` is meant to replace [`match.call`] and [`sys.call`];
#' its advantage is that it captures the environments bound to
#' arguments in addition to their written form.
#'
#' @return `get_call` returns a [dots] object, the first element of
#'   which represents the function name and [calling
#'   environment](caller).
#' @seealso do dots caller
#' @export
#' @param env An environment belonging to a currently executing
#'   function call. By default, the [caller] (so `get_call()` is
#'   equivalent to `get_call(environment())`.)
#' @param ifnotfound What to return if the call is not found. By
#'   default an error is thrown.
#' @examples
#' # We might think of re-writing the start of [lm] like so:
#' LM <- function(formula, data, subset, weights, na.action, method = "qr",
#'                model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
#'                contrasts = NULL, offset, ...) {
#'   cl <- get_call()
#'   mf <- do(model.frame,
#'            arg_list(formula, data, subset, weights, na.action, offset))
#'
#'   z <- get.call()
#'
#'   class(z) <- c("LM", class(z))
#'   z$call <- cl
#'   z
#' }
#'
#' # and `update` like so:
#' update.LM <- function(object, formula., ...) {
#'   call <- object$call
#'   extras <- dots(...)
#'   call$formula <- forced_quo(update.formula(formula(object), formula.))
#'   do(call)
#' }
get_call <- function(env = caller(environment()),
                     ifnotfound = stop("get_call: environment not found on stack")) {
  frame <- which_frame(env, ifnotfound)

  rho <- caller(env)
  call <- sys.call(frame)
  head <- call[[1]]
  fn <- sys.function(frame)
  argnames <- names(formals(fn))
  c.dots(quo_(head, rho),
         env2dots(env, argnames));
}

#' (get_call)
#'
#' `get_function(env)` finds the function object associated with a
#' currently executing call.
#'
#' `get_function` is similar to [`sys.function`], but is keyed by
#' environment rather than number.
#' @return `get_function` returns a closure.
#' @rdname get_call
#' @export
get_function <- function(env = caller(environment()),
                         ifnotfound = stop("get_function: environment not found on stack")) {
  sys.function(which_frame(env, ifnotfound))
}

which_frame <- function(env, ifnotfound) {
  frames <- sys.frames()
  where <- which(vapply(frames, identical, FALSE, env))
  if (length(where) == 0) {
    ifnotfound
  } else {
    where[1]
  }
}
