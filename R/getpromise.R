`%||%` <- function(a, b) if (is.null(a)) b else a

#' Fetch an argument from an environment by name.
#'
#' @param ... Variable names (unevaluated). Arguments may be named; these names
#' determine the names on the dots list (and not the variable names)
#' @return a \code{\link{dots}} object containing the promises that are bound to
#' those variables in the calling environment.
#' @note The tags on the dots object are determined by argument names;
#' variable names are discarded.
#' @seealso dots get_dots
#' @rdname arg_list
#' @export
#' @useDynLib nse _arg_dots
#' @useDynLib nse _dotsxp_to_flist
args <- function(...) {
  d <- unpack(dots(...))
  dts <- .Call(`_arg_dots`, d$envir, d$expr, d$name)
  .Call(`_dotsxp_to_flist`, dts)
}

#' Fetch an argument by name from an environment.
#'
#' @param name the name to look up. For \code{arg} this is a symbol
#'   and not evaluated; for \code{arg_} this is a symbol or
#'   character.
#' @rdname arg
#' @return A [quo] object.
#' @export
arg <- function(name) {
  pr <- arg_(quote(name), environment())
  arg_(body(pr), environment(pr))
}

#' ..
#' @param env  The environment to look in.
#' @rdname arg
#' @export
#' @useDynLib nse _arg
arg_ <- function(name, env) {
  .Call(`_arg`, env, as.name(name), TRUE)
}

#' \code{arg_dots_} is a normally evaluating version of args.
#'
#' @rdname arg_list
#' @param names A character vector or list of names.
#' @param envs An environment, or list of environments, to look for
#'   the bindings in.
#' @param tags An optional character vector specifying output variable names.
#' @export
#' @useDynLib nse _arg_dots
#' @useDynLib nse _dotsxp_to_flist
args_ <- function(names,
                  envs,
                  tags = names(names) %||% vapply(names, as.character, "")) {
  names <- lapply(names, as.name)
  if (!is.list(envirs)) (envirs = rep(list(envirs), length(names)))
  dts <- .Call(`_arg_dots`, envirs, names, tags, TRUE)
  .Call(`_dotsxp_to_flist`, dts)
}

#' Get environment or expression from a named argument.
#'
#' \code{arg_env} determines the lexical scope of an argument bound in
# the present environment.
#' @rdname arg_env
#' @param name A single argument name; not evaluated.
#' @param env The environment to look for the argument name in.
#' @export
#' @useDynLib nse _arg_env
arg_env <- function(name,
                    env=arg_env(name, environment())) {
  .Call(`_arg_env`, env, substitute(name), TRUE)
}

#' @export
#' @useDynLib nse _arg_env
arg_env_ <- function(name,
                     envir=arg_env(name, environment())){
  .Call(`_arg_env`, envir, as.name(name), TRUE)
}


#' ...
#'
#' \code{arg_expr(x)} is a shorthand for expr(arg(x)). It fetches the
#' expression attached to a promise in some environment.
#' environment. The effect is similar to \code{substitute(name)}.
#'
#' @rdname arg_env
#' @export
#' @useDynLib nse _arg_expr
arg_expr <- function(name,
                     envir=arg_env(name, environment())) {
  .Call(`_arg_expr`, envir, substitute(name), TRUE)
}

#' ...
#'
#' \code{arg_expr_} is the normally evaluating version of arg_expr.
#' @rdname arg_env
#' @export
#' @useDynLib nse _arg_expr
arg_expr_ <- function(name, envir=arg_env(name, environment()))
{
 .Call(`_arg_expr`, envir, as.name(name), TRUE)
 }

#' ...
#'
#' \code{is_promise} returns TRUE if a named variable is bound to a
#' promise. It returns a boolean vector with one entry for each name
#' given. An error is raised if a binding does not exist.
#'
#' @rdname arg_env
#' @export
#' @param ... Any number of variable names; not evaluated.
is_promise <- function(...) {
  d <- dots(...)
  is_promise_(exprs(d), envs(d))
}

#' ...
#'
#' \code{is_promise_} is a normally evaluating version of \code{is_promise}.
#' @rdname arg_env
#' @param names names of arguments to look up.
#' @export
#' @useDynLib nse _is_promise
is_promise_ <- function(names, envirs)
{
 mapply(FUN=function(name, envir) .Call(`_is_promise`, envir, name, TRUE),
            lapply(names, as.name),
            if (is.list(envirs)) envirs else list(envirs))
 }

#' Detect if named arguments are missing.
#'
#' `is_missing(...)` is similar to "missing" but returns a named
#' logical vector.
#'
#' \code{is_missing_} is a normally evaluating equivalent of
#' \code{\link{is_missing}}. It takes a number of names and environments,
#' and checks whether the names are bound to missing arguments.
#'
#' @rdname arg_env
#' @export
#' @useDynLib nse _is_missing
is_missing_ <- function(names, envirs) {
  names <- lapply(names, as.name)
  if (!is.list(envirs)) envirs <- list(envirs)
  mapply(function(name, envir) .Call(`_is_missing`, envir, name, TRUE),
         names,
         envirs)
}

#' ...
#'
#' \code{is_forced} returns FALSE if an argument is bound to a promise that
#' has not yet been forced, TRUE otherwise. An error is raised if a binding
#' does not exist.
#'
#' @rdname arg_env
#' @export
is_forced <- function(...) {
  d <- dots(...)
  is_forced_(exprs(d), envs(d))
}

#' ...
#'
#' \code{is_forced_} is a normally evaluating version of \code{is_forced}.
#' @rdname arg_env
#' @export
#' @useDynLib nse _is_forced
is_forced_ <- function(names, envirs) {
  mapply(FUN=function(name, envir) .Call(`_is_forced`, envir, name, TRUE),
         lapply(names, as.name),
         if(is.list(envirs)) envirs else list(envirs))
}

#' ...
#'
#' \code{is_literal} returns TRUE if a binding is (or could be) a
#' source literal. This includes singleton vectors and missing
#' values. (Depending on optimization settings, R will often not
#' bother constructing promises to wrap a literal.)
#' @rdname arg_env
#' @export
is_literal <- function(...) {
  d <- dots(...)
  is_literal_(exprs(d), envs(d))
}

#' ...
#'
#' \code{is_literal_} is a normally evaluating version of \code{is_literal}.
#' @rdname arg_env
#' @export
#' @useDynLib nse _is_literal
is_literal_ <- function(names, envirs, warn=TRUE) {
  mapply(FUN=function(name, envir) {
    .Call(`_is_literal`, envir, as.name(name), TRUE)
  },
  names,
  if (is.list(envirs)) envirs else list(envirs))
}

is_missing <- function(...) {
  d <- dots(...)
  is_missing_(exprs(d), envs(d))
}

is_missing_ <- function(exprs, envs) {
  mapply(FUN=function(name, envir) {
    .Call(`_is_missing`, envir, as.name(name), TRUE)
  },
  names,
  if (is.list(envs)) envs else list(envs))
}
