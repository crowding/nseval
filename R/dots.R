#' Capture a number of unevaluated arguments as an object.
#'
#' A dots object represents a named list of [quotation]s. It mirrors R's
#' special variable `...`. Unlike `...`, a `dots` is:
#' * immutable (evaluating does not change it),
#' * first-class (you can give it any name, not just `...`),
#' * data (The R interpreter treates it as literal data rather than
#'   triggering argument splicing).
#'
#' `d <- dots(...)` captures the contents of `...` without triggering
#' evaluation, and returns a list of class `"dots"`, each element of
#' which is a `[quotation]`. This improves on
#' `substitute(list(...))[[2]]` by capturing the context of each
#' expression along with the expressions.
#'
#' `d <- dots(foo, quux=bar+baz)` captures all of the given arguments
#' in a dots object, like `[alist]`, but also captures the
#' environment of each argument (The present environment in this case).
#'
#' @param ... Any number of arguments.
#' @return A list with class 'dots', each element of which is a [quotation].
#' @examples
#'
#' named.list <- function(...) {
#'  # Collect only named arguments, ignoring unnamed arguments.
#'  d <- dots(...)
#'  do(list, d[names(d) != ""])
#' }
#'
#' named.list(a=1, b=2*2, stop("this is not evaluated"))
#' @export
dots <- function(...) {
  get_dots(environment())
}

#' @export
as.dots.quotation <- function(x) {
  structure(list(x), class="dots")
}

#' `as.data.frame.dots` transforms the contents of a [dots] object,
#' into a data frame with one row per [quotation], with columns:
#'  * `name`: a character,
#'  * `expr`: an expression,
#'  * `env`: an [environment] object or NULL if [forced],
#'  * `value`: NULL or a value if forced.
#'
#' @note The columns have a class [oneline] for better printing.
#' @return `as.data.frame.dots` returns a data frame.
#' @param x A \code{\link{dots}} object.
#' @rdname dots
#' @export
#' @useDynLib nse _dots_unpack
as.data.frame.dots <- function(x) {
  x <- .Call(`_dots_unpack`, x)
  class(x$envir) <- "oneline"
  class(x$expr) <- "oneline"
  class(x$value) <- "oneline"
  attr(x, "row.names") <- make.unique(x$name)
  x
}

#' @rdname dots
#' @return `dots_(exprs, envs)` directly constructs a dots object
#'   given lists of expresions and environments.
#' @param exprs An expression or list of expressions.
#' @param envs An environment or list of environments.
#' @export
dots_ <- function(exprs, envs) {
  if (!is.list(exprs)) {
    exprs <- list(exprs)
  }
  if (!is.list(envs)) {
    envs <- list(envs)
  }
  structure(mapply(FUN=quo_, exprs, envs, SIMPLIFY=FALSE), class="dots")
}


#' Access or mutate quotation or dots objects.
#' \code{exprs(dots(...))} extracts a list of expressions, one per element
#' of a \code{\link{dots}} object.
#' @param x A dots object (see \code{\link{dots}}).
#' @return A named list of expressions. The mutator \code{exprs<-}
#'   returns a new dots object with the new expressions.
#' @rdname dots
#' @export
exprs <- function(x) UseMethod("exprs")

#' @export
#' @rdname dots
exprs.dots <- function(d) {
  lapply(unclass(d), function(x) .Call("_expr_quotation", x))
}

#' @export
#' @param value A replacement value.
#' @rdname dots
`exprs<-` <- function(x, value) {
  UseMethod("exprs<-")
}

#' @export
#' @rdname dots
`exprs<-.dots` <- function(x, value) {
  structure(mapply(FUN=quo_, SIMPLIFY=FALSE,
                   expr = value, env = envs(x)), class="dots")
}

#' (dots)
#'
#' `envs(dots(...)` extracts a list of environments from a
#' [dots] object.
#'
#' @rdname dots
#' @export
envs <- function(x) {
  UseMethod("envs")
}

#' @rdname dots
#' @return `envs(x)` returns a list of the environments of each
#'   quotation in x.
#' @param x a `[dots]` object.
#' @rdname dots
#' @export
envs.dots <- function(x) {
  lapply(x, environment)
}

#' @rdname dots
#' @return `envs<-` returns a list of new quotations with updated
#'   environments.
#' @param value A replacement list.
#' @rdname dots
#' @export
`envs<-` <- function(x, value) {
  UseMethod("envs<-")
}

#' @export
#' @return `envs<-` returns a list of new quotations with updated
#'   expressions.
#' @rdname dots
`envs<-.dots` <- function(x, value) {
  structure(mapply(FUN=quo_,
                   expr=exprs(x),
                   env=value,
                   SIMPLIFY = FALSE),
            class="dots")
}

#' @export
#' @rdname dots
`[.dots` <- function(x, ..., drop=FALSE) {
  y <- NextMethod("[")
  structure(y, class="dots")
}

#' @export
#' @rdname dots
`[<-.dots` <- function(x, ..., value)
{
  if (!is.null(value)) value <- as.dots(value)
  y <- NextMethod("[")
  structure(y, class="dots")
}

#' @export
#' @rdname dots
forced.dots <- function(d) {
  lapply(d, forced)
}

#' @export
#' @rdname dots
c.dots <- function(...) {
  l <- list(...)
  subdots <- lapply(l, function(x) unclass(as.dots(x)))
  structure(unlist(subdots, recursive=FALSE), class="dots")
}

#' Set or get the contents of "..." in an environment.
#'
#' `get_dots()` is equivalent to [`dots(...)`] or [`args( (...) )`].
#'
#' @param env The environment to look in.
#' @param inherits Whether to allow '...' to be inherited from enclosing
#'   environments.
#' @return `get_dots` returns the contents of `...` converted to a
#'   [dots] object.
#' @seealso env2dots
#' @export
#' @useDynLib nse _get_dots
#' @useDynLib nse _dotsxp_to_flist
get_dots <- function(env = caller(environment()), inherits=FALSE) {
  dts <- .Call(`_get_dots`, env, inherits)
  .Call(`_dotsxp_to_flist`, dts)
}

#' @rdname get_dots
#' @param d a `[dots]` object.
#' @param append if TRUE, the values should be appended to the
#'   existing binding. If false, existing binding for "..." will be
#'   replaced.
#' @return `set_dots` returns the updated environment, invisibly.
#' @seealso "%<-%" dots2env quo2env
#' @useDynLib nse _set_dots
#' @useDynLib nse _flist_to_dotsxp
#' @export
set_dots <- function(env, d, append=FALSE) {
  d <- as.dots(d)
  if (append) {
    d = c(get_dots(env), d);
  }
  .Call(`_set_dots`, .Call(`_flist_to_dotsxp`, d), env)
  invisible(env)
}
