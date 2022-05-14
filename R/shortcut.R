#' Get information about currently bound arguments.
#'
#' These are shortcut methods for querying current bindings.  For
#' example, `arg_env(x)` is equivalent to `env(arg(x))`,
#' `is_forced(x, y)` is equivalent to `forced(arg_list(x,y))`,
#' `dots_exprs(...)` is equivalent to `exprs(dots(...))`, and so
#' on. The shortcut forms skip the construction of the
#' intermediate [quotation] objects.
#'
#' As throughout this package, the functions with bare names will
#' quote their first argument literally. Their counterparts, with
#' underscores `_` at the end, take a character vector or list of
#' names.  and an environment to look in.
#' @rdname shortcut
#' @param sym For plain `arg_env`, etc, a bare name, which is
#'   quoted. For the underscore versions `arg_env_`, something that
#'   evaluates to a name or character.
#' @param env The environment to search in.
#' @return `arg_env` returns an environment.
#' @export
arg_env <- function(sym,
                    env = arg_env_(quote(sym), environment())) {
  sym_ <- arg_expr_(quote(sym), environment())
  arg_env_(sym_, env)
}

#' @rdname shortcut
#' @export
#' @useDynLib nseval _arg_env
arg_env_ <- function(sym,
                     env = arg_env_(quote(sym), environment())) {
  .Call("_arg_env", env, as.name(sym), TRUE)
}

#' @rdname shortcut
#' @export
#' @useDynLib nseval _arg_expr
#' @return `arg_expr` returns the expression bound to a named argument.
arg_expr <- function(sym,
                     env=arg_env_(quote(sym), environment())) {
  sym_ <- arg_expr_(quote(sym), environment())
  arg_expr_(sym_, env)
}

#' @rdname shortcut
#' @export
#' @useDynLib nseval _arg_expr
arg_expr_ <- function(sym,
                      env=arg_env_(quote(sym), environment())) {
  .Call("_arg_expr", env, as.name(sym), TRUE)
}

#' @export
#' @rdname shortcut
#' @useDynLib nseval _dots_envs
#' @useDynLib nseval _get_dots
dots_envs <- function(...) {
  .Call("_dots_envs", .Call("_get_dots", environment(), FALSE))
}

#' @export
#' @description `dots_exprs(...)` quotes its arguments and returns a
#'   list of expressions. It is equivalent to `exprs(dots(...))` (and
#'   is nearly equivalent to `alist(...)`, one difference being that
#'   dots_exprs will expand `...`.)
#' @rdname shortcut
#' @useDynLib nseval _dots_exprs
#' @useDynLib nseval _get_dots
dots_exprs <- function(...) {
  .Call("_dots_exprs", .Call("_get_dots", environment(), FALSE))
}

#' @rdname shortcut
#' @export
#' @return `is_forced` and other `is_*` return a logical vector with
#'   optional names.
is_forced <- function(...) {
  d <- dots(...)
  is_forced_(exprs(d), envs(d))
}

#' @rdname shortcut
#' @export
#' @useDynLib nseval _is_forced
is_forced_ <- function(syms, envs) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
    syms,
    if(is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call("_is_forced", env, as.name(sym), TRUE)
    })
}

#' @description `is_literal(x)` returns TRUE if an argument `x` could be a
#'   source literal. Specifically it tests whether `x` is bound to a
#'   singleton vector or a [missing_value]. This check happens without
#'   forcing `x`.
#' @rdname shortcut
#' @export
is_literal <- function(...) {
  d <- dots(...)
  is_literal_(exprs(d), envs(d))
}

#' @rdname shortcut
#' @export
#' @param envs An environment or list of environments.
#' @useDynLib nseval _is_literal
is_literal_ <- function(syms, envs) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call("_is_literal", env, as.name(sym), TRUE)
    })
}


#' @rdname shortcut
#' @export
#' @description `is_missing(...)` checks whether an argument is missing,
#'   without forcing. It is similar to [missing] but can take
#'   multiple arguments, and can be called in more situations, such as
#'   from a nested inner function.
is_missing <- function(...) {
  d <- dots(...)
  is_missing_(exprs(d), envs(d))
}

#' @rdname shortcut
#' @param syms A character vector or list of symbols.
#' @param unwrap Whether to recursively [unwrap] before testing for
#'   missingness.
#' @export
#' @useDynLib nseval _is_missing
#' @description `is_missing_(syms, envs)` is a normally evouating version
#'   of is_missing.  `syms` should be a symbol, character vector or
#'   list of such. `envs` should be an environment, or list of
#'   environments. Vector recycling rules apply, so you can call with
#'   a vector of names and one env, or vice versa.
is_missing_ <- function(syms, envs, unwrap=TRUE) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  if (!is.list(envs)) envs <- list(envs)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call("_is_missing", env, as.name(sym), FALSE, unwrap)
    })
}

#' @description `is_promise` returns TRUE if the given variable is bound to
#'   a promise. Not all arguments are bound to promises; byte-compiled
#'   code often omits creating a promise for literal or missing arguments.
#' @rdname shortcut
#' @export
#' @param ... Bare variable names (for `is_*_`) or expressions (for
#'   `dots_*`). Not forced.
is_promise <- function(...) {
  is_promise_(dots_exprs(...), dots_envs(...))
}

#' @rdname shortcut
#' @export
#' @useDynLib nseval _is_promise
is_promise_ <- function(syms, envs)
{
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call("_is_promise", env, as.name(sym), TRUE)
    })
}

#' @description
#' `is_default`  determines whether an argument is bound to the
#' function's default value for that argument. It must be called
#' before the arguments have been forced (afterwards it will return
#' FALSE).
#' @rdname shortcut
#' @export
is_default <- function(...) {
  is_default_(dots_exprs(...), dots_envs(...))
}

#' @rdname shortcut
#' @export
is_default_ <- function(syms, envs = arg_env(syms)) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      (    identical(arg_env_(sym, env),
                     env)
        && identical(arg_expr_(sym, env),
                     formals(get_function(env))[[sym]]))
    })
}
