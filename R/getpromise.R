`%||%` <- function(a, b) if (is.null(a)) b else a

#' Fetch a list of promises by name.
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
#' @useDynLib promises _arg_dots
arg_dots <- function(...) {
  d <- unpack(dots(...))
  dts <- .Call(`_arg_dots`, d$envir, d$expr, d$name)
  .Call(`_dotsxp_to_flist`, dts)
}

#' Fetch a single promise by name from an environment.
#'
#' @param name the name to look up. For \code{arg_promise} this is a symbol
#'   and not evaluated; for \code{arg_promise_} this is a symbol or
#'   character.
#' @rdname arg_promise
#' @return A promise object.
#' @export
arg_promise <- function(name) {
  pr <- arg_promise_(quote(name), environment())
  arg_promise_(body(pr), environment(pr))
}

#' ..
#' @param env  The environment to look in.
#' @rdname arg_promise
#' @useDynLib promises _arg_promise
#' @export
arg_promise_ <- function(name, env) {
  .Call(`_arg_promise`, env, as.name(name), TRUE)
}

#' \code{arg_dots_} is a normally evaluating version of arg_dots.
#'
#' @rdname arg_list
#' @param names A character vector or list of names.
#' @param envirs An environment, or list of environments, to look for
#'   the bindings in.
#' @param tags An optional character vector specifying output variable names.
#' @export
arg_dots_ <- function(names,
                      envirs,
                      tags = names(names) %||% vapply(names, as.character, "")) {
  names <- lapply(names, as.name)
  if (!is.list(envirs)) (envirs = rep(list(envirs), length(names)))
  dts <- .Call(`_arg_dots`, envirs, names, tags, TRUE)
  .Call(`dotsxp_to_flist`, dts)
}

#' Get environment or expression from a named argument.
#'
#' \code{arg_env} determines the lexical scope of an argument bound in
# the present environment.
#' @rdname arg_env
#' @param name A single argument name; not evaluated.
#' @param envir The environment to look for the argument name in.
#' @useDynLib promises _arg_env
#' @export
arg_env <- function(name,
                    envir=arg_env(name, environment())) {
  .Call(`_arg_env`, envir, substitute(name), TRUE)
}

#' @export
arg_env_ <- function(name,
                     envir=arg_env(name, environment())){
  .Call(`_arg_env`, envir, as.name(name), TRUE)
}

#' ...
#'
#' \code{arg_expr} fetches the expression attached to an argument in the given
#' environment. The effect is similar to \code{substitute(name)} but more
#' specific.
#'
#' @rdname arg_env
#' @useDynLib promises _arg_expr
#' @export
arg_expr <- function(name,
                     envir=arg_env(name, environment())) {
  .Call(`_arg_expr`, envir, substitute(name), TRUE)
}

#' ...
#'
#' \code{arg_expr_} is the normally evaluating version of arg_expr.
#' @rdname arg_env
#' @export
arg_expr_ <- function(name, envir=arg_env(name, environment())) {
  .Call(`_arg_expr`, envir, as.name(name), TRUE)
}

#' ...
#'
#' \code{is_promise} returns TRUE if a named variable is bound to a
#' promise. It returns a boolean vector with one entry for each name
#' given. An error is raised if a binding does not exist.
#'
#' @rdname arg_env
#' @useDynLib promises _is_promise
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
is_promise_ <- function(names, envirs) {
  mapply(FUN=function(name, envir) .Call(`_is_promise`, envir, name, TRUE),
         lapply(names, as.name),
         if (is.list(envirs)) envirs else list(envirs))
}

#' Detect if arguments are missing.
#'
#' \code{missing_} is a normally evaluating equivalent of
#' \code{\link{missing}}. It takes a number of names and environments,
#' and checks whether the names are bound to missing arguments.
#'
#' @rdname arg_env
#' @export
#' @useDynLib promises _is_missing
missing_ <- function(names, envirs) {
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
#' \code{is_lazy_} is a normally evaluating version of \code{is_lazy}.
#' @useDynLib promises _is_forced
#' @rdname arg_env
#' @export
is_forced_ <- function(names, envirs) {
  mapply(FUN=function(name, envir) .Call(`_is_forced`, envir, name, TRUE),
         lapply(names, as.name),
         if(is.list(envirs)) envirs else list(envirs))
}

#' ...
#'
#' \code{is_literal} returns TRUE if a binding is (or could be) a
#' source literal. This includes singleton vectors and missing
#' values. (R will often not bother constructing promises when a
#' function is called a literal in source.)
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
#' @useDynLib promises _is_literal
#' @export
is_literal_ <- function(names, envirs, warn=TRUE) {
  mapply(FUN=function(name, envir) {
    .Call(`_is_literal`, envir, as.name(name), TRUE)
  },
  names,
  if (is.list(envirs)) envirs else list(envirs))
}
