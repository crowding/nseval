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
#' @export
as.dots <- function(x) {
  UseMethod("as.dots")
}

#' @export
as.dots.dots <- identity

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


#' `as.dots.literal` turns a list of data values into a dots object
#'   consisting of forced promises. (A forced promise records a value
#'   and an expression, but not an environment.)
#' @param x a list.
#' @rdname as.dots
#' @export
#' @useDynLib nse _quotation_literal
as.dots.literal <- function(values) {
  structure(lapply(as.list(values),
                   function(x) .Call(`_quotation_literal`, x)),
            class="dots")
}

#' Copy bindings from an environment into a dots object.
#'
#' @param env An environment.
#' @param names Which names to extract from the environment. By
#'   default extracts the bindings present at root level.
#' @param include_missing Whether to include missing bindings.
#' @param use_dots Whether to unpack the contents of `...`.
#' @return A \link{dots} object.
#' @export
#' @useDynLib nse _env_to_dots
env2dots <- function(env,
                     names = ls(envir = env, all.names = TRUE),
                     include_missing = TRUE,
                     expand_dots = TRUE)
{
  x <- .Call(`_env_to_dots`, env, names, include_missing, expand_dots)
}

filter <- function(x, pred) x[pred(x)]
goodname <- function(x) !(x %in% c(NA_character_, "", "..."))

#' Take quotations from a dots object and inject them into an environment.
#'
#' All named entries in the dots object will be bound to
#' variables. Unnamed entries will be appended to any existing value
#' of `...` in the order in which they appear.
#'
#' @param d A named dots object to use.
#' @param names Which variables to populate in the environment. If
#'   NULL is given,  will use all names present in the dotlist.  If a
#'   name is given that does not match any names from the dots object,
#'   an error is raised.
#' @param env Specify an environment object to populate and return. By
#'   default a new environment is created.
#' @param use_dots Whether to bind unnamed or unmatched items to
#'   \code{...}. If FALSE, these items are discarded. If TRUE, they
#'   bound to \code{...} in the environment. If items have duplicate
#'   names, the earlier ones are used and the rest placed in "...".
#' @param append if `TRUE`, unmatched or unnamed items will be
#'   appended to any existing value of '...'. If `FALSE`, the existing
#'   binding of `...` will be cleared. (Neither happens if `use_dots`
#'   is FALSE.)
#' @param hash if \code{env} is NULL, this argument is passed to
#'   \code{\link{new.env}}.
#' @param size if \code{env} is NULL, this argument is paseed to
#'   \code{\link{new.env}}.
#' @param parent if \code{env} is NULL, this argument is paseed to
#'   \code{\link{new.env}}.
#' @return An environment object.
#' @aliases as.environment.dots
#' @export
#' @useDynLib nse _flist_to_dotsxp
#' @useDynLib nse _dots_to_env
dots2env <- function(d,
                     env = new.env(hash = hash, parent = parent, size = size),
                     names = NULL,
                     use_dots = TRUE,
                     append = TRUE,
                     hash = (length(dots) > 100),
                     size = max(29L, length(dots)),
                     parent = emptyenv()) {
  if (is.null(names)) {
    names <- filter(names(d) %||% c(), goodname)
  }
  if (use_dots) {
    m <- match(names, names(d) %||% c())
    if (any(is.na(m))) {stop("Named variable(s) requested but not present in dotlist.")}
    picked <- d[m]
    d[m] <- NULL
    if (append) {
      d <- c(get_dots(env), d)
    }
    picked <- .Call(`_flist_to_dotsxp`, picked)
    extras <- .Call(`_flist_to_dotsxp`, d)
    .Call(`_dots_to_env`, picked, env, extras)
  } else {
    picked <- d[names]
    picked <- .Call(`_flist_to_dotsxp`, d)
    .Call(`_dots_to_env`, picked, env, NULL)
  }
}

#' Bind a name in an environment to a promise.
#' @param q a quotation.
#' @param env The environment to store in.
#' @param name The name to use. If "" or NULL, will append to "...".
quo2env <- function(q, env, name) {
  q <- as.quo(q)
  d <- as.dots(q)
  if (!is.null(name)) {
    names(d) <- as.character(name)
  }
  dots2env(d, env)
}

#' Explicitly create closure objects.
#' @export
#' @param args Either NULL, or a named list of default value
#'   expressions (which may be missing_value() to indicate no
#'   default). [alist] is useful for this.
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
as.quo.quotation <- identity

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
as.quo.quotation <- identity

#' @export
as.quo.dots <- function(x) {
  if (length(x) == 1)
    x[[1]]
  else
    stop("can't convert nonscalar dots to a quotation")
}

#' @export
as.quo.default <- function(x) {
  if (mode(x) == "list") {
    expr <- x$expr
    env <- x$env
  } else {
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
