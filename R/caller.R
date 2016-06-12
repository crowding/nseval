#' Get the calling environment of a given environment.
#' 
#' @param envir The environment to inspect. By default, the environment from
#'   which \code{caller} was called.
#'   
#' @return The environment in which the function call that created \code{envir} 
#'   was called. If that environment cannot be reliably determined, an error is 
#'   thrown.
#'   
#'   In R, expressions are evaluated in environments; environments are created 
#'   by function invocation. In the code
#'   
#'   \code{X <- environment()
#'   F <- function() {
#'     Y <- environment()
#'     caller(Y)
#'   } 
#'   F()}
#'   
#'   the environment bound to "Y" was created by calling F(), and that call occurs
#'   in the environment bound to "X". We say that X is the calling environment of
#'   Y.
#'   
#'   Note that the "caller" is in general different from the "enclosing
#'   environment" which is returned by \code{\link{parent.env}}). It is also
#'   different from the environment in the previous frame of the execution
#'   stack as returned by "parent.frame." In the case of lazy evaluation, 
#'   an environment may be reactivated to compute a lazy argument; the code
#'   in the lazy argument should have the same 'caller' as other code in its
#'   lexical environment, though it has a different activation path.
#'   
#'   \code{caller} is intended as a replacement for most uses of
#'   \code{\link{parent.frame}}. Unlike \code{parent.frame}, \code{caller} will
#'   throw an error if it cannot determine the calling environment. This tends
#'   to happen in situations involving lazy evaluation, closures, \code{eval} or
#'   \code{do.call}. In these same situaions, \code{parent.frame} can return
#'   incorrect results (i.e. environments that are actually not the calling
#'   environment) without warning, which the author finds to be a frequent 
#'   source of bugs.
#'   
#'   Whenever possible, use \code{\link{arg_env}} instead of \code{caller}. Use 
#'   \code{caller} for NSE functions that take no arguments, or to work around 
#'   bad NSE in other people's code.
#'   
#'   \code{caller()} with no arguments should always give the same results as 
#'   \code{caller(environment())}, but the former amounts to calling \code{caller} twice
#'   (as \code{caller} must discover its own caller) so the latter will be faster.
#'   
#' @export
#' 
#' @examples
#' # For further examples please see the test cases
#' # located under inst/tests/test-caller.R
caller <- function(envir=caller(environment())) {
  frames <- sys.frames()
  where <- which(vapply(frames, identical, FALSE, envir))
  
  if (length(where) == 0) {
    stop("Caller: environment not found on stack")
  }
  
  if (is.primitive(sys.function(where[1]))) {
    stop("caller: calling function is eval or other primitive")
  }
  
  parents <- sys.parents()
  whichparent <- parents[where[1]]
  
  if (whichparent == where[1]) {
    stop("Caller: caller is no longer on stack")
  }
  
  frames[[whichparent]]
}

# TODO: replacement for match.call.
this_call <- function() {
  stop("Implement me")
}

#' Wrap a function so that it will see a particular caller or parent.frame.
#'
#' @param f The function to call. A name or character string may be
#' used, which should refer to a function visible in the environment
#' given.
#'
#' @param envir The environment to issue the call from. If \code{\link{caller}}
#' is called within \code{f}, it will return \code{envir}.
#'
#' @return A function wrapper. Calling it will call `f`, but from within `f` a 
#' call to `parent.frame()` or `caller` will return `envir`. 
#' 
#' call_from(f, e)("foo", "bar") is similar to `eval(quote(f("foo", "bar")), envir=e)`
#' except that the argument list is applied normally instead of being evaluated 
#' in the target environment.
#' 
#' A typical use for `with_caller` is to gain some flexibility working with impolite
#' functions that manipulate their `caller()` or `parent_frame()`
#' @useDynLib fexpr _make_call
#' @export
with_caller <- function(f, envir=arg_env(f)) {
  force(envir)
  function(...) .Call(`_make_call`, f, envir, dots(...))
}
