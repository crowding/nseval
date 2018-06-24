#' Find the environment which defines a name.
#'
#' `locate` starts at a given environment, and searches enclosing
#' environments for a name. It returns the first which defines `sym`.
#' @param sym A name. For `locate` the argument is used literally. For
#'   `locate_` it should be a [name] or list of names.
#' @param env Which environment to begin searching from.
#' @param mode Either `"any"` or `"function"`. `"any"` finds the
#'   lowest enclosing environment which gives any definiton for `sym`.
#'   `"function"` searches for an environment which defines `sym` as a
#'   function. This may force lazy arguments in the process, in the
#'   same way as [get].
#' @param ... Further arguments passed to methods.
#' @return An environment object which defines `sym`, if one is found.
#' @examples
#' # Here is how to implement R's `<<-` operator, using `locate_`:
#' `%<<-%` <- function(lval, rval) {
#'  lval_ <- arg(lval)
#'  rval_ <- arg(rval)
#'  target.env <- locate_(expr(lval_), parent.env(env(lval_)))
#'  #note that `<-` is a primitive which requires its lvalue and call
#'  #head to come from teh same env
#'  env(lval_) <- target.env
#'  do_(quo(`<-`, target.env), lval_, rval_)
#' }
#'
#' x <- "not this one"
#' local({
#'   x <- "this one"
#'   local({
#'     x <- "not this one either"
#'     x %<<-% "this works like builtin <<-"
#'   })
#'   print(x)
#' })
#' @export
locate <- function(sym,
                   env = arg_env_(quote(sym), environment()),
                   mode = "any",
                   ...) {
  sym_ <- arg_expr_(quote(sym), environment())
  locate_(sym = sym_, env = env, mode = mode, ...)
}

#' `locate_` is the normally evaluating method; `locate(x)` is
#' equivalent to `locate_(quo(x))` or `locate_(quote(x), environment())`.
#' @rdname locate
#' @export
locate_ <- function(sym,
                    env = arg_env_(quote(sym), environment()),
                    mode = "any", ...) {
  # Default arguments vs. s3 dispatch pitfall!
  # because UseMethod is going to force "sym",
  # which would make output of arg_env_ invalid,
  # I need to force "env" first...
  force(env)
  # and dispatch from a call with no default args
  locate.dispatch(sym=sym, env=env, mode=mode, ...)
}

locate.dispatch <- function(sym, env, mode, ...) {
  UseMethod("locate_")
}


#' If `sym` is a list (of [name]s) or a [dots] object, `locate_(sym)`
#' returns a list.
#' @rdname locate
#' @export
locate_.list <- function(sym,
                         env = arg_env_(quote(sym), environment()),
                         mode = "any", ...) {
  force(env)
  lapply(sym, locate_, env=env, mode=mode, ...)
}

#' When `sym` is a [quotation] or [dots], any `env` argument is ignored.
#' @rdname locate
#' @export
locate_.quotation <- function(sym,
                              env = "ignored",
                              mode = "any",
                              ...) {
  locate_(sym=expr(sym), env=env(sym), mode = mode, ...)
}

#' @rdname locate
#' @note If you use a literal character argument, as in `locate("x",
#'   environment())`, you must also provide the environment argument
#'   explicitly; `locate("x")` won't work in compiled
#'   functions. However using a literal name like `locate(x)` will
#'   work OK. See note under [arg].
#' @export
locate_.character <- function(sym,
                              env = arg_env_(quote(sym), environment()),
                              mode = "any",
                              ...) {
  if (length(sym) == 1) {
    locate_.name(sym=as.name(sym), env=env, mode=mode, ...)
  } else {
    # The "intuitive" thing here might be to return a list:
    # lapply(FUN=locate_.character, x, env=env, mode=mode, ...)
    # However this would not be type-stable (is "x" a vector of length
    # 1 or a singleton?)
    stop("use locate_.list for character vectors")
  }
}

# not exported.
`locate_.call` <- function(sym,
                           env = arg_env_(quote(sym), environment()),
                           mode = "any",
                           ...) {
  locate_(sym[[2]], env=env, mode=mode)
}

#' @export
#' @rdname locate
`locate_.(` <- `locate_.call`


#' @export
#' @rdname locate
locate_.dots <- function(sym,
                         env = "ignored",
                         mode = "any",
                         ...) {
  structure(lapply(sym, locate_, ...), class="dots")
}

#' @rdname locate
#' @param ifnotfound What is returned if the symbol is not found. By
#'   default an exception is raised.
#' @useDynLib nse _locate
#' @export
locate_.name <- function(sym,
                         env = arg_env_(quote(sym), environment()),
                         mode = "any",
                         ifnotfound = stop("Binding ", deparse(sym), " not found")) {
  .Call("_locate",
        sym,
        env,
        switch(mode,
               "any" = FALSE,
               "function" = TRUE)
        ) %||% ifnotfound
}

#' Unwrap variable references.
#'
#' Given an un[forced] [quotation] whose expression is a bare variable
#' name, `unwrap` follows the variable reference, and returns a
#' quotation. When the argument is forced or has a nontrivial
#' expression `unwrap` has no effect.
#'
#' The syntax `locate( (...) )` is available for locating `...`.
#'
#' There are two good use cases for `unwrap(x, recursive=TRUE)`. One
#' is to derive plot labels (the most inoccuous use of
#' metaprogramming). Another is to check for missingness (this is what
#' R's [missing] and does as well).
#'
#' Using `unwrap(x, recursive=TRUE)` in other situations can get you
#' into confusing situations -- effectively you are changing the
#' behavior of a parent function that may be an unknown number of
#' levels up the stack, possibly turning a standard-evaluating
#' function into nonstandard-evaluating function. So recursive
#' unerapping is not the default behavior.
#' @export
#' @param x a [quotation] to unwrap.
#' @param recursive Default `FALSE` unwraps exactly once. If
#'   `TRUE`, unwrap as far as possible (until a forced promise or
#'   nontrivial expression is found.)
#' @return The [quotation] method returns a [quotation].
#' @examples
#' # different levels of unwrapping:
#' f <- function(x) { g(x) }
#' g <- function(y) { h(y) }
#' h <- function(z) {
#'   print(arg(z))
#'   print(unwrap(quo(z)))
#'   print(unwrap(unwrap(quo(z))))
#'   print(unwrap(quo(z), recursive=TRUE))
#' }
#'
#' w <- 5
#' f(w)

unwrap <- function(x, recursive=FALSE) {
  UseMethod("unwrap")
}

#' @export
unwrap.quotation <- function(x, recursive=FALSE) {
  .Call("_unwrap_quotation", x, recursive)
}


#' @export
#' @rdname unwrap
#' @return The [dots] method returns a dots object with each quotation unwrapped.
unwrap.dots <- function(x, recursive=FALSE) {
  structure(lapply(x, function(x) .Call("_unwrap_quotation", x, recursive)), class="dots")
}
