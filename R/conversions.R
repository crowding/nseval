#' Convert a list of expressions into a \code{\dots} object (a list of
#' promises.)
#'
#' @param x a vector or list.
#' @param env The environment to attach to each expression.
#'
#' @return An object of class \code{\dots}. For \code{as.dots}, the
#'   list items are treated as data values, and create already-forced
#'   promises. For \code{as.dots.exprs}, values are treated as the
#'   expressions of new unforced promises.
#' @seealso dots "%<<%" "%<<<%" "%()%" "[.dots" "[[.dots",
#'   "names.dots"
#' @export
as.dots <- function(x) {
  UseMethod("as.dots")
}

#' @export
as.dots.dots <- function(x) x

#' @export
as.dots.list <- function(x)
{
  structure(mapply(FUN=as.quo, x), class="dots")
}

#' Capture promises present in an environment and construct a dots object.
#'
#' All bindings in the environment (but not those from its parents) will be
#' copied into a new \code{\dots} list. Bindings that are promises
#' will be added to the \dots list without forcing, while other
#' bindings will be wrapped in an already-forced promise.  If `...` is
#' bound in the environment, all bindings it contains will be added to
#' the \dots list. The output will not be in any particular order.
#'
#' @export
#' @aliases dots2env
as.dots.environment <- function(x) env2dots(x)

#' @export
as.dots.dots <- function(x) x

is.sequence <- function(x) is.vector(x) || is.list(x)

#' @export
as.dots.default <- function(x) {
  if(is.sequence(x)) {
    as.dots.list(as.list(x))
  } else {
    stop("can't convert this into a dots")
  }
}

#' @export
as.dots.lazy_dots <- function(x)
{
  structure(lapply(x, as.quo), class="dots")
}

#' Convert a list of expressions into a dots object.
#'
#' @param exprs a list of expressions
#' @param env An environment or list of environments.
#. @return a \link{dots} object with the given expressions.
#' @export
as.dots.exprs <- function(exprs, env = arg_env(exprs)) {
  force(env)
  dots_(exprs = exprs, envs = env)
}


#' \code(as.dots.literal) injects a list of literal values into a dots
#'   object consisting of forced promises. (A forced promise records a
#'   value and an expression, but not an environment.)
#' @param x a list.
#' @rdname as.dots
#' @export
#' @useDynLib nse _dotsxp_to_flist
#' @useDynLib nse _as_dots_literal
as.dots.literal <- function(values) {
  .Call(`_dotsxp_to_flist`, .Call(`_as_dots_literal`, as.list(values)))
}

#' ...
#' @export
#' @param env An environment.
#' @param names Which names to extract from the environment. By
#'   default extracts all bindings.
#' @param include_missing Whether to include "missing" bindings.
#' @param expand_dots Whether to unpack the contents of `...`.
#' @return A \link{dots} object.
#' @useDynLib nse _env_to_dots
env2dots <- function(env,
                     names = ls(envir = env, all.names = TRUE),
                     include_missing = TRUE,
                     expand_dots = TRUE)
{
  x <- .Call(`_env_to_dots`, env, names, include_missing, expand_dots)
}

filter <- function(x, pred) x[pred(x)]
goodname <- function(x) !(x %in% c(NA_character_, ""))

#' Convert a dots object into promises bound in an envrionment.
#'
#' All named entries in the dots object will be bound to
#' variables. Unnamed entries will be appended to any existing value
#' of `...` in the order in which they appear.
#'
#' @param dots The dots object to convert.
#' @param names Which variables to populate in the
#'   environment. By default, will use all names present in the dotlist.
#'   If a name is given that does not appear in the dots object, an error is
#'   raised.
#' @param parent If creating a new environment, its parent (see
#'   \code{\link{new.env}}).
#' @param envir If not NULL, an environment object to populate and
#'   return. If NULL, a new environment will be created.
#' @param with_dots Whether to bind unnamed or unmatched items to
#'   \code{...}. If FALSE, these items are discarded. If TRUE, they
#'   are appended to any existing \code{...} in the environment. If
#'   arguments have duplicate names, the earlier ones are used and the
#'   rest placed in "...".
#' @param hash if \code{envir} is non-NULL; See \code{\link{new.env}}.
#' @param size if \code{envir} is non-NULL; See \code{\link{new.env}}.
#' @param parent if \code{envir} is non-NULL; See
#'   \code{\link{new.env}}.
#' @return An environment object.
#' @aliases as.environment.dots
#' @export
#' @useDynLib nse _flist_to_dotsxp
#' @useDynLib nse _dots_to_env
dots2env <- function(dots,
                     names = NULL,
                     with_dots = TRUE,
                     hash = (length(dots) > 100),
                     size = max(29L, length(dots)),
                     parent = emptyenv(),
                     envir = new.env(hash = hash, parent = parent, size = size)) {
  if (is.null(names)) {
    names <- filter(names(dots) %||% c(), goodname)
  }
  if (with_dots) {
    m <- match(names, names(dots) %||% c())
    if (any(is.na(m))) {stop("Named variable(s) not present in dotlist.")}
    dotlist <- .Call(`_flist_to_dotsxp`, dots[m])
    dots[m] <- NULL
    extras <- .Call(`_flist_to_dotsxp`, dots)
    .Call(`_dots_to_env`, dotlist, envir, extras)
  } else {
    dots <- dots[names]
    dotlist <- .Call(`_flist_to_dotsxp`, dots)
    .Call(`_dots_to_env`, dotlist, envir, NULL)
  }
}

#' Explicitly create closure objects.
#' @export
#' @param args Either NULL, or a named list of default value expressions
#'   (which may be missing_value() to indicate no default).
#' @param body An expression for the body of the function.
#' @param env The environment to create a function from.
function_ <- function(args, body, env = caller(environment())) {
  f <- do.call("function", list(as.pairlist(args), body), envir=environment())
  environment(f) <- env
  f
}

#' @export
as.quo <- function(x) {
  UseMethod("as.quo")
}

#' @export
as.quo.function <- function(x) {
  if (is.primitive(x)) stop("can't convert primitive to quotation")
  f <- formals(x)
  if (length(f) != 0) {
    stop("can only convert function to quotation if it has no args")
  }
  quo_(body(x), environment(x))
}

#' @export
as.quo.quo <- function(x) x

#' @export
as.quo.default <- function(x) {
  if (mode(x) == "list") {
    expr <- x$expr
    env <- x$env
  } else {
    expr <- x
    stop(paste0("can't convert ", class(x)[1] ," to a quo"))
  }
  quo_(expr, env)
}

#' @export
as.quo.formula <- function(x) {
  expr <- x[[2]]
  env <- attr(x, ".Environment")
  quo_(expr, env)
}

#' @export
as.quo.lazy <- function(x) {
  quo_(x$expr, x$env)
}

#' @export
as.lazy_dots <- function(x, env) {
  UseMethod("as.lazy_dots")
}

#' @export
as.lazy_dots.dots <- function(x)
{
  lazyeval::lazy_dots %()% x
}

#' @export
as.environment.dots <- function(dots) {
  dots2env(dots)
}
