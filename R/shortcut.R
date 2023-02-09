#' Get information about currently bound arguments.
#'
#' These are shortcut methods for querying current bindings.  For
#' example, `arg_env(x)` is equivalent to `env(arg(x))`,
#' `is_forced(x, y)` is equivalent to `forced(arg_list(x,y))`,
#' `dots_exprs(...)` is equivalent to `exprs(dots(...))`, and so
#' on. The shortcut forms skip the construction of the
#' intermediate [quotation] objects.
#'
#' Throughout this package, some functions come in two forms, a "bare"
#' version which quotes its first argument literally, and a
#' normally-evaluating version with a trailing underscore in its
#' name. So `is_forced(x)` chiecks whether "x" is a missing variable,
#' while `is_forced_(x, environment())` checks whether "x" contains
#' the _name_ of another variable which is missing. The following are
#' all equivalent:
#'
#' * `arg_env(x)`
#' * `{y <- quo(x); arg_env_(y)}`
#' * `arg_env_(quote(x), environment())`
#' * `arg_env_(quo(x))`
#' * `env(arg_(quo(x)))`.
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


#' @rdname shortcut
#' @export
#' @param ifnotforced What to return if calling arg_value on a promise
#'   that has not been forced.
#' @useDynLib nseval _arg_value
#' @return `arg_value` returns the value bound to a named argument.
arg_value <- function(sym,
                     env=arg_env_(quote(sym), environment()),
                     ifnotforced=stop("Variable is not forced, so has no value")) {
  sym_ <- arg_expr_(quote(sym), environment())
  arg_value_(sym_, env, ifnotforced)
}

#' @rdname shortcut
#' @export
#' @useDynLib nseval _arg_value
arg_value_ <- function(sym,
                       env=arg_env_(quote(sym), environment()),
                       ifnotforced=stop("Variable is not forced, so has no value")) {
  sigil <- function() sym
  result <- .Call("_arg_value", env, as.name(sym), TRUE, sigil)
  if (missing(result)) return(missing_value())
  if (identical(result, sigil)) ifnotforced
  else result
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
  is_forced_.default(exprs(d), envs(d))
}

#' @rdname shortcut
#' @export
#' @useDynLib nseval _is_forced
is_forced_ <- function(syms, envs) {
  UseMethod("is_forced_")
}

#' @exportS3Method is_forced_ default
is_forced_.default <- function(syms, envs) {
  out <- structure(logical(length(syms)),
                   names=names(syms) %||% as.character(syms))
  if (is.list(envs)) {
    for (i in seq_along(syms))
      out[[i]] <- .Call("_is_forced", envs[[i]], as.name(syms[[i]]), FALSE)
  } else {
    for (i in seq_along(syms))
      out[[i]] <- .Call("_is_forced", envs, as.name(syms[[i]]), FALSE)
  }
  out
}

#' @exportS3Method is_forced_ name
is_forced_.name <- function(syms, envs) {
  .Call("_is_forced", envs, syms, FALSE)
}

#' @exportS3Method is_forced_ quotation
is_forced_.quotation <- function(syms, ...) {
  .Call("_is_forced", env(syms), expr(syms), FALSE)
}

#' @exportS3Method is_forced_ dots
is_forced_.dots <- function(syms, ...) {
  mapply(exprs(syms),
         envs(syms),
         FUN=function(sym, env) {
           .Call("_is_forced", env, as.name(sym), FALSE)
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
  is_literal_.default(exprs(d), envs(d))
}

#' @rdname shortcut
#' @export
#' @param envs An environment or list of environments.
#' @useDynLib nseval _is_literal
is_literal_ <- function(syms, envs) {
  UseMethod("is_literal_")
}

#' @exportS3Method is_literal_ default
is_literal_.default <- function(syms, envs) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call("_is_literal", env, as.name(sym), FALSE)
    })
}

#' @exportS3Method is_literal_ quotation
is_literal_.quotation <- function(syms, ...) {
  .Call("_is_literal", env(syms), expr(syms), FALSE)
}

#' @exportS3Method is_literal_ dots
is_literal_.dots <- function(syms, ...) {
  mapply(exprs(syms),
         envs(syms),
         FUN=function(sym, env) {
           .Call("_is_literal", env, as.name(sym), FALSE)
         })
}

#' @rdname shortcut
#' @export
#' @description `is_missing(...)` checks whether an argument is
#'   missing, without forcing. It is similar to [missing] but can take
#'   multiple arguments, and can be called in more situations, such as
#'   from a nested inner function.
#' @details When used with quotation objects, the `is_*_` functions
#'   with trailing underscore work at one level of indirection
#'   compared to quotation methods. For example, `missing_(x)` tests
#'   whether `expr(x)` is `[missing_value()]`, whereas `is_missing_(x)`
#'   assumes `expr(x)` is a _name_ and checks if that name refers to a
#'   variable that is missing. The following are equivalent:
#'
#'   * `is_missing(x)`
#'   * `is_missing_(quo(x))`
#'   * `missing_(arg(x))`
is_missing <- function(...) {
  d <- dots(...)
  is_missing_.default(exprs(d), envs(d))
}

#' @rdname shortcut
#' @param syms A character vector or list of symbols.
#' @param unwrap Whether to recursively [unwrap] before testing for
#'   missingness.
#' @export
#' @useDynLib nseval _is_missing
#' @description `is_missing_(syms, envs)` is a normally evaluating version
#'   of is_missing.  `syms` should be a symbol, character vector or
#'   list of such. `envs` should be an environment, or list of
#'   environments. Vector recycling rules apply, so you can call with
#'   a vector of names and one env, or vice versa.
#' @details When used with a `quotation` or `dots`, is_missing(q)
#' looks for the variable(s) specified by expr(q) in environment env(q)]`.
is_missing_ <- function(syms, envs, unwrap=TRUE) {
  UseMethod("is_missing_")
}

#' @exportS3Method is_missing_ default
is_missing_.default <- function(syms, envs, unwrap=TRUE) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call("_is_missing", env, as.name(sym), FALSE, unwrap)
    })
}

#' @exportS3Method is_missing_ quotation
#' @rdname shortcut
is_missing_.quotation <- function(syms, ..., unwrap=TRUE) {
  .Call("_is_missing", env(syms), expr(syms), FALSE, unwrap)
}

#' @exportS3Method is_missing_ dots
is_missing_.dots <- function(syms, ..., unwrap=TRUE) {
  mapply(exprs(syms),
         envs(syms),
         FUN=function(sym, env) {
           .Call("_is_missing", env, as.name(sym), FALSE, unwrap)
         })
}


#' @description `is_promise` returns TRUE if the given variable is bound to
#'   a promise. Not all arguments are bound to promises; byte-compiled
#'   code often omits creating a promise for literal or missing arguments.
#' @rdname shortcut
#' @export
#' @param ... Bare variable names (for `is_*`) or expressions (for
#'   `dots_*`). Not forced.
is_promise <- function(...) {
  is_promise_.default(dots_exprs(...), dots_envs(...))
}

#' @rdname shortcut
#' @export
#' @useDynLib nseval _is_promise
is_promise_ <- function(syms, envs) {
  UseMethod("is_promise_")
}

#' @exportS3Method is_promise_ default
is_promise_.default <- function(syms, envs) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call("_is_promise", env, as.name(sym), FALSE)
    })
}

#' @exportS3Method is_promise_ quotation
#' @rdname shortcut
is_promise_.quotation <- function(syms, ...) {
  .Call("_is_promise", env(syms), expr(syms), FALSE)
}

#' @exportS3Method is_promise_ dots
is_promise_.dots <- function(syms, ...) {
  mapply(exprs(syms),
         envs(syms),
         FUN=function(sym, env) {
           .Call("_is_promise", env, as.name(sym), FALSE)
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
  is_default_.default(dots_exprs(...), dots_envs(...))
}

#' @rdname shortcut
#' @export
is_default_ <- function(syms, envs) {
  UseMethod("is_default_")
}

is_default__ <- function(sym, env) {
  env <- locate_(sym, env)
  (    identical(arg_env_(sym, env),
                 env)
    && identical(arg_expr_(sym, env),
                 formals(get_function(env))[[sym]]))
}


#' @exportS3Method is_default_ default
is_default_.default <- function(syms, envs) {
    if (is.null(names(syms)))
      names(syms) <- as.character(syms)
    mapply(
      syms,
      if (is.list(envs)) envs else list(envs),
      FUN=is_default__)
}

#' @exportS3Method is_default_ quotation
#' @rdname shortcut
is_default_.quotation <- function(syms, ...) {
  is_default__(env(syms), expr(syms))
}

#' @exportS3Method is_default_ dots
is_default_.dots <- function(syms, ...) {
  mapply(exprs(syms),
         envs(syms),
         FUN=is_default__)
}
