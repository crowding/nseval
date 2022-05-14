#' Dots objects: lists of quotations.
#'
#' `d <- dots(a = one, b = two)` captures each of its arguments,
#' unevaluated, in a dots object (a named list of [quotation]s).
#'
#' Objects of class "dots" mirror R's special variable `...`.
#' Unlike `...`, a `dots` is:
#' * immutable (evaluating does not change it),
#' * first-class (you can give it any name, not just `...`),
#' * data (The R interpreter treates it as literal data rather
#' than triggering argument splicing).
#'
#' `d <- dots(...)` is used to capture the contents of `...` without
#' triggering evaluation. This improves on
#' `as.list(substitute(...()))` by capturing the environment of each
#' argument along with their expressions. (You can also use
#' [`get_dots()`].)
#'
#' @param ... Any number of arguments.
#' @return `dots(...)` constructs a list with class 'dots', each
#'   element of which is a [quotation].
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

#' @rdname dots
#' @return `dots_(exprs, envs)` constructs a dots object given lists
#'   of expressions and environments. (To construct a dots object from
#'   quotation objects, use [`c()`].)
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


#' @param d A [dots] object.
#' @return `exprs(d)` extracts a list of expressions from a dots object.
#' @rdname dots
#' @export
exprs <- function(d) UseMethod("exprs")

#' @export
#' @rdname dots
exprs.dots <- function(d) {
  lapply(unclass(d), function(x) .Call("_expr_quotation", x))
}

#' @return The mutator `exprs(d) <- value` returns a new dots object
#'   with the new expressions.
#' @export
#' @rdname dots
`exprs<-` <- function(d, value) {
  UseMethod("exprs<-")
}

#' @export
#' @rdname dots
`exprs<-.dots` <- function(d, value) {
  structure(mapply(FUN=quo_, SIMPLIFY=FALSE,
                   expr = value, env = envs(d)), class="dots")
}

#' @return `envs(d)` extracts a list of environments from a dots
#'   object.
#' @rdname dots
#' @export
envs <- function(d) {
  UseMethod("envs")
}

#' @return `envs(d)` returns a named list of environments.
#' @rdname dots
#' @export
envs.dots <- function(d) {
  lapply(d, environment)
}

#' @return `envs(d) <- value` returns an updated dots object with the
#'   environments replaced with the new value(s).
#' @rdname dots
#' @param value A replacement value or list of values.
#' @export
`envs<-` <- function(d, value) {
  UseMethod("envs<-")
}

#' @rdname dots
#' @export
`envs<-.dots` <- function(d, value) {
  structure(mapply(FUN=quo_,
                   expr=exprs(d),
                   env=value,
                   SIMPLIFY = FALSE),
            class="dots")
}

#' @export
#' @rdname dots
#' @param drop See [Extract].
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
c.dots <- function(...) {
  l <- list(...)
  subdots <- lapply(l, function(x) unclass(as.dots(x)))
  structure(unlist(subdots, recursive=FALSE), class="dots")
}

#' @export
#' @rdname dots
c.quotation <- c.dots

#' Set or get the contents of `...`.
#'
#' `get_dots()` unpacks `...` from a given environment and returns a
#'   [dots] object.
#'
#' `get_dots()` is equivalent to `dots(...)` or
#' ``arg_list(`...`)``.
#'
#' @param env The environment to look in.
#' @param inherits Whether to pull `...` from enclosing environments.
#' @return `get_dots` returns a [dots] list. If `...` is not bound or
#'   is missing, it returns an empty dots list.
#' @seealso env2dots set_arg dots2env
#' @export
#' @useDynLib nseval _get_dots
#' @useDynLib nseval _dotsxp_to_flist
get_dots <- function(env = caller(environment()), inherits=FALSE) {
  dts <- .Call("_get_dots", env, inherits)
  .Call("_dotsxp_to_flist", dts)
}

#' @rdname get_dots
#' @description
#' `set_dots` takes a [dots] list and uses it to create a binding for
#' `...` in a given environment.
#' @param d a `[dots]` object.
#' @param append if TRUE, the values should be appended to the
#'   existing binding. If false, existing binding for "..." will be
#'   replaced.
#' @return `set_dots` returns the updated environment, invisibly.
#' @useDynLib nseval _set_dots
#' @useDynLib nseval _flist_to_dotsxp
#' @export
set_dots <- function(env, d, append=FALSE) {
  d <- as.dots(d)
  if (append) {
    d = c(get_dots(env), d);
  }
  .Call("_set_dots", .Call("_flist_to_dotsxp", d), env)
  invisible(env)
}
