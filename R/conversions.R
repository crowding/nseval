#' @description
#' `as.data.frame.dots` transforms the contents of a [dots] object
#' into a data frame with one row per [quotation], with columns:
#'  * `name`: a character,
#'  * `expr`: an expression,
#'  * `env`: an [environment] object or NULL if [forced],
#'  * `value`: NULL or a value if forced.
#' @note The columns have a class `"oneline"` for better printing.
#' @return `as.data.frame.dots` returns a data frame.
#' @param x A \code{\link{dots}} object.
#' @param row.names If not given, uses `make.unique(x$name)`
#' @rdname dots
#' @export
#' @useDynLib nseval _dots_unpack
as.data.frame.dots <- function(x, row.names = NULL, ...) {
  x <- .Call("_dots_unpack", x)
  class(x$envir) <- "oneline"
  class(x$expr) <- "oneline"
  class(x$value) <- "oneline"
  attr(x, "row.names") <- make.unique(row.names %||% x$name)
  x
}

#' Convert items into quotations or dots.
#'
#' `as.dots` is a generic function for converting data into [dots].
#' @param x a vector or list.
#' @return An object of class \code{\dots}.
#' @export
#' @rdname as.dots
as.dots <- function(x) {
  UseMethod("as.dots")
}

#' @export
#' @rdname as.dots
as.dots.dots <- identity


#' @export
#' @rdname as.dots
as.dots.quotation <- function(x) {
  structure(list(x), class="dots")
}

#' @export
#' @rdname as.dots
as.dots.list <- function(x)
{
  structure(mapply(FUN=as.quo, x), class="dots")
}

#' @description
#' `as.dots.environment` is a synonym for [`env2dots`].
#' @export
#' @rdname as.dots
#' @seealso env2dots
#' rdname dots2env
as.dots.environment <- function(x) env2dots(x)

#' @export
#' @rdname as.dots
as.dots.lazy_dots <- function(x)
{
  structure(lapply(x, as.quo), class="dots")
}

#' Copy bindings from an environment into a dots object, or vice versa.
#'
#' `env2dots` copies all bindings in the environment (but not those
#' from its parents) into a new [dots] object. Bindings that are
#' promises will be captured without forcing. Bindings that are not
#' promises will be rendered as [forced] quotations. The output will
#' not be in any guaranteed order.
#'
#' @param env An environment.
#' @param names Which names to extract from the environment. By
#'   default extracts all bindings present in `env`, but not in
#'   its enclosing environments.
#' @param include_missing Whether to include missing bindings.
#' @param expand_dots Whether to include the contents of `...`.
#' @return A \link{dots} object.
#' @export
#' @useDynLib nseval _env_to_dots
env2dots <- function(env = caller(environment()),
                     names = ls(envir = env, all.names = TRUE),
                     include_missing = TRUE,
                     expand_dots = TRUE)
{
  .Call("_env_to_dots", env, names, include_missing, expand_dots)
}


#' Make or update an environment with bindings from a dots list.
#'
#' All named entries in the dots object will be bound to
#' variables. Unnamed entries will be appended to any existing value
#' of `...` in the order in which they appear.
#'
#' @param x A [dots] object with names.
#' @param names Which variables to populate in the environment. If
#'   NULL is given, will use all names present in the dotlist.  If a
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
#' @seealso env2dots
#' @export
#' @useDynLib nseval _flist_to_dotsxp
#' @useDynLib nseval _dots_to_env
dots2env <- function(x,
                     env = new.env(hash = hash, parent = parent, size = size),
                     names = NULL,
                     use_dots = TRUE,
                     append = TRUE,
                     hash = (length(dots) > 100),
                     size = max(29L, length(dots)),
                     parent = emptyenv()) {
  if (is.null(names)) {
    names <- filter(names(x) %||% c(), goodname)
  }
  if (use_dots) {
    m <- match(names, names(x) %||% c())
    if (any(is.na(m))) {stop("Named variable(s) requested but not present in dotlist.")}
    picked <- x[m]
    x[m] <- NULL
    if (append) {
      x <- c(get_dots(env), x)
    }
    picked <- .Call("_flist_to_dotsxp", picked)
    extras <- .Call("_flist_to_dotsxp", x)
    .Call("_dots_to_env", picked, env, extras)
  } else {
    picked <- x[names]
    picked <- .Call("_flist_to_dotsxp", x)
    .Call("_dots_to_env", picked, env, NULL)
  }
}

#' @export
#' @rdname dots2env
as.environment.dots <- function(x) {
  dots2env(x)
}

is.sequence <- function(x) is.vector(x) || is.list(x)

#' @export
#' @rdname as.dots
as.dots.default <- function(x) {
  if(is.sequence(x)) {
    as.dots.list(as.list(x))
  } else {
    stop("can't convert this into a dots")
  }
}

filter <- function(x, pred) x[pred(x)]
goodname <- function(x) !(x %in% c(NA_character_, "", "..."))


#' Explicitly create closures.
#'
#' `function_` is a normally-evaluating version of [`function`], which
#' creates closures. A closure object has three components: the
#' argument list, the body expression, and the enclosing environment.
#'
#' @param args The argument list of the new function. NULL is accepted
#'   to make a function with no arguments. Arguments are specified as
#'   a named list; the list names become the argument names, and the
#'   list values become the default expressions. A value of
#'   [missing_value()] indicates no default. [alist] and [arglist] are
#'   useful for making argument lists.
#' @param body An expression for the body of the function.
#' @param env The enclosing environment of the new function.
#' @return A closure.
#' @seealso environment formals body
#' @export
#' @examples
#' f1 <- function(x, y = x) { x + y }
#' f2 <- function_(alist(x = , y = x),
#'                 quote( { x + y } ),
#'                 environment())
#' identical(f1, f2) # TRUE
#'
#' # `fn` makes a compact way to write functions;
#' # `fn(x+y)` is equivalent to `function(x, y) x+y`
#' fn <- function(exp) {
#'   exp_ <- arg(exp)
#'   nn <- arglist(all.names(expr(exp_), functions=FALSE))
#'   function_(nn, expr(exp_), env(exp_))
#' }
#'
#' fn(x^2)
#' fn(x+y)
function_ <- function(args, body, env = arg_env(args, environment())) {
  f <- do.call("function", list(as.pairlist(args), body), envir=environment())
  environment(f) <- env
  f
}

#' @rdname function_
#' @description
#' `arglist()` is a helper that produces a named list of
#' [missing_value]s given a character vector of names.
#' @param names A character vector.
#' @param fill The expression (default missing)
#' @export
arglist <- function(names, fill = missing_value()) {
  structure(rep(list(fill), length(names)), names = names)
}

#' Compatibility conversions.
#'
#' Convert quotations and dot lists to the representations used
#' by other packages.
#' @rdname compat
#' @export
#' @param x a [dots] object.
#' @param env See [lazyeval::as.lazy_dots].
#' @return `as.lazy_dots` returns a [lazyeval::lazy_dots] object.
as.lazy_dots <- function(x, env) {
  UseMethod("as.lazy_dots")
}

#' @export
#' @rdname compat
as.lazy_dots.dots <- function(x, env="ignored", ...)
{
  set_dots(environment(), x)
  lazyeval::lazy_dots(...)
}

