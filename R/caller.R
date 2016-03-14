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
#' @useDynLib fexpr _caller
caller <- function(envir=caller(environment())) {
  st <- stacktrace::stacktrace()
  
  #Where is target environment instantiated?
  where <- which(vapply(st$cloenv, identical, FALSE, envir))
  
  nActive <- length(where)
  
  if (nActive == 0) {
    stop("Target environment not found on stack")
  }

  if (is.primitive(st$callfun[[where[1]]])) {
    stop("Target environment is a prmitive. Maybe it is called from do.call")
  }
  
  r <- st[[where[1], "sysparent"]]
  r
}


# TODO: replacement for match.call.
this_call <- function() {

}

#' Call a function from a particular environment.
#'
#' @param f The function to call. A name or character string may be
#' used, which should refer to a function visible in the environment
#' given.
#'
#' @param envir The environment to issue the call from. If \code{\link{caller}}
#' is called within \code{f}, it will return \code{envir}.
#'
#' @return A function wrapper. Normally one would immediately invoke it, as in
#' call_from(f, e)("foo", "bar", "baz").
#'
#' You can also use this to insulate yourself from poorly behaved functions that
#' use parent.frame() when they should use arg_env(), as in \link{plyr::here}.
call_from <- function(f, envir) {
  function(...) make_call(f, envir, args)
}

#' Wrap a function so that it will see a particular caller or parent.frame.
#'
#' Writing \code{call_here(f)} is equivalent to writing
#' \code{call_from(f, environment())}.
call_here <- function(f) {
  here <- arg_env(f)
  function(...) make_call(f, here, dots(...))
}

make_call <- function(f, envir, args) {
  do.call(`(`, envir, list(f, args))
}
