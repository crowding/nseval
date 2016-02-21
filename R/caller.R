#' Get the caller of an environment (also an environment).
#'
#' @param envir The environment to inspect. By default,
#' the environment from whicn \code{caller} was called.
#'
#' @return The environment in which the function call that created \code{envir}
#' was called. If that environment cannot be reliably determined, an error is
#' thrown.
#'
#' Whenever possible, use \code{\link{arg_env}} instead of \code{caller}. Use
#' \code{caller} for NSE functions that take no arguments, or to work around
#' bad NSE in other people's code.
#'
#' In R, expressions are evaluated in environments; environments are created
#' by function invocation. In the code
#'
#' X <- environment()
#' F <- function() {
#'  Y <- environment()
#'  caller(Y)
#' }
#' F()
#'
#' The environment labaled as Y is created by evaluating the function F(), which
#' occurs in the environment labeled X. We say that X is the caller of Y.
#'
#' Note that the "caller" is in general different from the "enclosing environment"
#' which is returned by \code{\link{parent.env}}). It is also different from the
#' environment in the previous frame of the execution stack. In the case of lazy
#' evaluation, an environment may be reactivated to compute a lazy argument;
#' the code in the lazy argument should have the same 'caller' as other code in
#' its lexical environment, though it has a different activation path.
#'
#' \code{caller} is a replacement for most uses of \code{\link{parent.frame}}.
#' There are  several situations, involving closures and/or lazy evaluation, in which
#' \code{parent.frame} returns incorrect results, or results that are change depending
#' on what time parent.frame is evaluated.The value returned by caller() should always
#' remain the same for any particular environment (although caller may throw an error
#' if the caller can no longer be determined.).
#'
#' @export
#'
#' @examples
#' # For further examples please see the test cases
#' # located under inst/tests/test-caller.R
#' @useDynLib fexpr _caller
caller <- function(envir=caller(environment())) {
  do.call(parent.frame, envir=envir, list())
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
