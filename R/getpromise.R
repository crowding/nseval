`%||%` <- function(a, b) if (is.null(a)) b else a

#' Fetch promises associated with named arguments.
#'
#' @param ... Variable names (unevaluated). Arguments may be named; these names
#' determine the names on the dots list (and not the variable names)
#' @return a \code{\link{dots}} object containing the promises that are bound to
#' those variables in the calling environment.
#' @author Peter Meilstrup
#' @note The tags on the dots object are determined by argument names;
#' variable names are discarded.
#' @seealso dots get_dots
#' @rdname arg_list
#' @export
#' @useDynLib promises _arg_dots
arg_dots <- function(...) {
  d <- unpack(dots(...))
  .Call(`_arg_dots`, d$envir, d$expr, d$name)
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
  .Call(`_arg_dots`, envirs, names, tags, TRUE)
}

#' Get environment or expression from a named argument.
#'
#' \code{arg_env} determines the lexical scope of an argument (which must be an
#' un-evaluated promise).
#' @rdname arg_env
#' @param name A single argument name; not evaluated.
#' @param envir The environment to look for the argument name in.
#' @useDynLib promises _arg_env
#' @export
arg_env <- function(name,
                    envir=arg_env(name, environment())) {
  .Call(`_arg_env`, envir, substitute(name), TRUE)
}

#` @export
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
#' @param ... Unquoted variable names.
is_promise <- function(...) {
  is_promise_(dots_expressions(...), dots_environments(...))
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

#' Tell if arguments are missing.
#'
#' \code{missing_} is a normally exporting equivalent of
#' \code{\link{missing}}. It takes a number of names and environments,
#' and checks whether the names are bound to missing arguments. To
#' check whether values in a ... listor normal list are set to
#' missing, use \code{\link{is.missing}}.
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
  is_forced_(dots_expressions(...), dots_environments(...))
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
  is_literal_(dots_expressions(...), dots_environments(...))
}

#' ...
#'
#' \code{is_literal_} is a normally evaluating version of \code{is_literal}.
#' @rdname arg_env
#' @useDynLib promises _is_literal
#' @export
is_literal_ <- function(names, envirs, warn=TRUE) {
  mapply(FUN=function(name, envir) .Call(`_is_literal`, envir, name, TRUE),
         lapply(names, as.name),
         if (is.list(envirs)) envirs else list(envirs))
}

#' Convert an environment into a dots object, without forcing promises.
#'
#' All bindings in the environment will be copied into a new
#' \code{\dots} list. Bindings that are promises will be added to the
#' \dots list without forcing, while other bindings will be wrapped in
#' an already-forced promise.  If `...` is bound in the environment,
#' all bindings it contains will be added to the \dots list. The
#' output will not be in any particular order.
#'
#' @param envir An environment.
#' @param include_missing Whether to include "missing" bindings in the dotslist.
#' @return A \link{dots} object.
#' @useDynLib promises _env_to_dots
#' @export
env2dots <- function(envir, include_missing=FALSE) {
  .Call(`_env_to_dots`, envir, ls(envir=envir, all.names=TRUE), include_missing)
}

#' Convert an a ... object into an environment, without forcing promises.
#'
#' All named entries in the dots object will be bound to
#' variables in the given environment. Unnamed entries
#' will be appended to any existing value of `...` in the order
#' in which they appear. Promises will not be forced.
#'
#' @param dots The dots object to convert.
#' @param envir if not NULL, an environment to populate. If NULL, a new
#' environment will be created.
#' @param parent If creating a new environment, the parent to use.
#' @param size The size of the new environment.
#' @param hash Whether the new environment should use a hashtable.
#' @return An environment object.
#' @useDynLib promises _dots_to_env
#' @export
dots2env <- function(dots, envir = NULL,
                     parent = arg_env(dots, environment()),
                     hash = (length(dots) > 100),
                     size = max(29L, length(dots))) {
  force(parent)
  if (is.null(envir))
    envir <- new.env(hash = hash, parent = parent, size = size)

  .Call(`_dots_to_env`, dots, envir)
}
