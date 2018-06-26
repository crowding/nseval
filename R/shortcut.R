#' Get information about currently bound arguments.
#'
#' These are shortcut methods for querying current bindings.  For
#' example, `arg_env(x)` is equivalent to `env(arg(x))`,
#' `is_forced(x, y)` is equivalent to `forced(arg_list(x,y))`,
#' `dots_exprs(...)` is equivalent to `exprs(dots(...))`, and so
#' on. The shortcut forms, however, skip the construction of the
#' intermediate [quotation] objects.
#' @rdname shortcut
#' @param sym For `arg_env`, etc, a bare name (not forced). For the
#'   normally evaluating `arg_env_` and so on, [name].
#' @param env The environment to search in.
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
#' @return `arg_expr` extracts an expression from a named argument.
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
#' @rdname shortcut
#' @useDynLib nseval _dots_exprs
#' @useDynLib nseval _get_dots
#' @return `dots_exprs(...)` is equivalent to `exprs(dots(...))` which
#'   is nearly equivalent to `alist(...)`.
dots_exprs <- function(...) {
  .Call("_dots_exprs", .Call("_get_dots", environment(), FALSE))
}

#' @rdname shortcut
#' @export
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

#' @return `is_literal(x)` returns TRUE if `x` could be a source
#'   literal. Specifically this tests whether it is X is bound to a
#'   singleton vector or a [missing_value].
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
#' @return `is_missing(...)` checks whether a variable is missing,
#'   without forcing. It is similar to [missing] but can take
#'   multiple arguments.
is_missing <- function(...) {
  d <- dots(...)
  is_missing_(exprs(d), envs(d))
}

#' @rdname shortcut
#' @param syms A character vector or list of symbols.
#' @param recursive Whether to recursively [unwrap] before testing for
#'   missingness.
#' @export
#' @useDynLib nseval _is_missing
is_missing_ <- function(syms, envs, recursive=TRUE) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  if (!is.list(envs)) envs <- list(envs)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call("_is_missing", env, as.name(sym), FALSE, recursive)
    })
}

#' @return `is_promise` returns TRUE if the given variable is bound to
#'   a promise. Not all arguments are bound to promises; byte-compiled
#'   code often omits creating a promise for literal arguments
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

#' `is_default`  determines whether an argument is bound to the
#' function's default value for that argument. It must be called
#' before the arguments have been forced (afterwards it will return
#' FALSE).
#' @return `is_default` returns a logical vector
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
