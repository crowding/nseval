#' Find the caller of a given environment.
#'
#' Given an environment that is currently on the stack, `caller`
#' determines the calling environment.
#'
#' For example, in the code:
#'
#' ```
#' X <- environment()
#' F <- function() {
#'   Y <- environment()
#'   caller(Y)
#' }
#' F()
#' ```
#'
#' the environment called `Y` was created by calling `F()`, and that
#' call occurs in the environment called `X`. In this case `X` is the
#' calling environment of `Y`, so `F()` returns the same environment
#' as `X()`.
#'
#' `caller` is intended as a replacement for [parent.frame], which
#' returns the next environment up the calling stack -- which is
#' sometimes the same value, but differs in some situations, such as
#' when lazy evaluation re-activates an environment. `parent.frame()`
#' can return different things depending on the order in which
#' arguments are evaluated, without warning. `caller` will by default
#' throw an error if the caller cannot be determined.
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
#' @param n How many levels to work up the stack.
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
                   ifnotfound = stop("caller: environment not found on stack"),
                   n = 1) {
  ## I think we want to find the activation record that corresponds
  ## to the *earliest* invocation of our environment, and look at its
  ## sys.parent.
  ##
  ## Doing that from this side of Rinternals.h is tricky. Doubly so
  ## since sys.calls() and sys.frame() elide some of the activation
  ## records. I wrote a small package "stacktrace" which helped to
  ## figure this out.

  ## cat("getting caller of ", format(env), "\n")

  ## print(stacktrace(), max.width=80);
  ## print(data.frame(frames = oneline(I(as.list(sys.frames()))),
  ##                  parents = oneline(sys.parents()),
  ##                  calls = oneline(I(as.list(sys.calls())))),
  ##       max.width=80)
  where <- which_frame(env, return(ifnotfound)) #return...

  if (is.primitive(sys.function(where))) {
    if (is_default(ifnotfound)) {
      stop("caller: calling function is a primitive, which has no environment")
    } else {
      return(ifnotfound)
    }
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
    result <- do.call("parent.frame", list(), envir=env)
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
#' rest of the arguments are evaluated the same way as `do_`.
#'
#' The head, or first element of the call list, represents the
#' function, and it should evaluate to a function object. The rest of
#' the call list is used as that function's arguments.
#'
#' When a quotation is used as the first element, the call is evaluated
#' from the environment given in that quotation. This means that calls
#' to [caller()] (or `parent.frame()`) from within that function
#' should return that environment.
#'
#' `do` is intended to be a replacement for base function [do.call].
#' For instance these two lines are similar in effect:
#'
#'     do.call("complex", list(imaginary = 1:3))
#'     do(complex, dots(imaginary = 1:3))
#'
#' As are all these:
#'
#'     do.call("f", list(as.name("A")), envir = env)
#'     do_(quo(f, env), quo(A, env)):
#'     do_(dots_(list(as.name("f"), as.name("A")), env))
#'     do_(dots_(alist(f, A), env))
#'
#' @note When the environment of the call head differs from that of
#'   the arguments, `do` may make a temporary binding of `...` to pass
#'   arguments. This will cause some primitive functions, like (
#'   [`<-`], or [`for`]), to fail with an error like
#'   "'...' used an in incorrect context," because these primitives do
#'   not understand how to unpack `...`. To avoid the use of `...`,
#'   ensure that all args have the same environment as the call head,
#'   or are forced.
#'
#'   For the specific case of calling `<-`, you can use [`set_`] to
#'   make assignments.
#' @seealso get_call do.call match.call set_
#'
#' @param ... A function to call and list(s) of arguments to pass. All
#'   should be `quotation` or `dots` objects, except the first
#'   argument for `do` which is quoted literally.
#'
#' @return The return value of the call.
#' @export
do <- function(...) {
  d <- dots(...)
  d[[1]] <- forced_quo(arg(..1)) #quote first argument
  set_dots(environment(), d)
  d <- c.dots(...)
  do__(d)
}

#' @rdname do
#' @useDynLib nseval _construct_do_call
#' @useDynLib nseval _remove
#' @export
do_ <- function(...) {
  d <- c.dots(...)
  do__(d)
}

do__ <- function(d) {
  # returns list of 3: expr, env, temporary dotsxp
  toeval <- .Call("_construct_do_call", d)
  e <- toeval[[2]]
  if (!is.null(toeval[[3]])) {
    # make an ephemeral self-resetting binding for `...`
    if (exists("...", e, inherits=FALSE)) {
      olddots <- mget("...", e, inherits=FALSE)
      .Call("_remove", quote(...), e)
      delayedAssign(  # lazy so that it only resets once!
        "reset", {
          .Call("_remove", quote(...), e)
          .Call("_set_dots", olddots[[1]], envir=e)
        })
    } else {
      delayedAssign("reset", .Call("_remove", quote(...), e))
    }
    makeActiveBinding(quote(...), function() {reset; toeval[[3]]}, e)
    on.exit(reset) # if it hasn't already
  }
  eval(toeval[[1]], e)
}

#' Assign values to variables.
#'
#' `set_` is a normally-evaluating version of [`<-`].
#' `set_enclos_` is a normally evaluating version of
#' [`<<-`].
#' @param dest A [quotation] specifying the destination environment
#'   and name. This can also be an indexing, expression, and `set_` will
#'   perform subassignment.
#' @param val The value to assign.
#' @return `set_` returns `val`, invisibly.
#' @details `set_` differs from `[assign]` in that `set_` will process
#'   subassignments.
#'
#' These helpers are here because it is tricky to use [`do_`] with
#' [`<-`] (see Note under [do_]).
#' @examples
#' set_(quo(x), 12) #equivalent to `x <- 12`
#' set_(quo(x[3]), 12) #equivalent to `x[3] <- 12`
#' e <- new.env()
#' set_(quo(x[3], e), 12) #assigns in environment `e`
#' set_enclos_(quo(x[3], e), 12) #assigns in a parent of environment `e`
#' @export
set_ <- function(dest, val) {
  if (is.language(val)) {
    val <- as.call(list(`quote`, val))
  }
  do.call(`<-`, list(expr(dest), val), FALSE, env(dest))
}

#' @rdname set_
#' @export
set_enclos_ <- function(dest, val) {
  if (is.language(val)) {
    val <- as.call(list(`quote`, val))
  }
  do.call(`<<-`, list(expr(dest), val), FALSE, env(dest))
}

#' Get information about currently executing calls.
#'
#' `get_call(env)`, given an environment associated with a currently
#' executing call, returns the function call and its arguments, as a
#' [dots] object. To replicate a call, the [dots] object returned can
#' be passed to [do].
#'
#' `get_call` is meant to replace [`match.call`] and [`sys.call`];
#' its advantage is that it captures the environments bound to
#' arguments in addition to their written form.
#'
#' @return `get_call` returns a [dots] object, the first element of
#'   which represents the function name and [caller] environment.
#' @seealso do dots caller
#' @export
#' @param env An environment belonging to a currently executing
#'   function call. By default, the [caller] of get_call itself
#'   (so `get_call()` is equivalent to `get_call(environment())`.)
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
  argnames <- names(formals(fn)) %||% character(0)
  c.dots(quo_(head, rho),
         env2dots(env, argnames));
}

#' @description
#' `get_function(env)` finds the function object associated with a
#' currently executing call.
#' @details
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
