#' Quotation objects.
#'
#' `quo` captures its argument literally, that is, without evaluating,
#' and constructs a quotation.
#'
#' A quo (or quotation) `q <- quo( something )` has two parts: an
#' expression [expr(q)] with an environment [env(q)]. (Like in
#' writing, an 'expression' may simply be a set of words, but a
#' 'quotation' comes bundled with a citation, to reference a context
#' in which it was said.)
#'
#' A quo is parallel to a 'promise' which is the data structure R uses
#' to hold lazily evaluated arguments. A quo is different from a
#' promise because it is an immutable data object.
#'
#' As a data object, a quo does not automatically evaluate like a
#' promise, but can be evaluated explicitly with the methods [value]
#' or [force_].  A quo is immutable, so it does not mutate into a
#' "forced" state if you choose to evaluate it.
#'
#' A function can capture its arguments as quotations using [`arg`].
#'
#' A [dots] object is a list of quotations.
#'
#' @export
#' @param expr An expression. For `quo` this is taken literally and
#'   not evaluated. For `quo_` this is evaluated normally.
#' @param force Immediately evaluate the expression and create a
#'   "forced" quotation, i.e. one that stores an expression and value,
#'   but no environment.
#' @return `quo_` and `quo` return an object of class "quotation".
quo <- function(expr, env = arg_env_(quote(expr), environment()), force = FALSE) {
  quo_(arg_expr_(quote(expr), environment()), env = env, force = force)
}

#' `quo_(expr, env)` is the normally evaluating version. It
#' constructs a quotation given an expression and environment.
#' @rdname quo
#' @param expr An expression.
#' @param env An environment.
#' @export
#' @useDynLib nse _quotation
quo_ <- function(expr, env, force = FALSE) {
  if(force) {
    .Call(`_quotation`, NULL, expr, eval(expr, env));
  } else {
    .Call(`_quotation`, env, expr, missing_value());
  }
}

#' @rdname quo
#' @export
env <- function(q) UseMethod("env")

#' @rdname quo
#' @export
env.quotation <- function(q) {
  environment(q)
}

#' @rdname quo
#' @export
`env<-` <- function(q, value) {
  UseMethod("env<-")
}

#' @rdname quo
#' @export
`env<-.quotation` <- function(q, value) {
  quo_(expr(q), value);
}

#' @rdname quo
#' @export
expr <- function(q) UseMethod("expr")

#' @rdname quo
#' @export
#' @param q A quotation object.
expr.quotation <- function(q) {
  .Call("_expr_quotation", q)
}

#' @rdname quo
#' @export
`expr<-` <- function(q, value) {
  UseMethod("expr<-")
}

#' @rdname quo
#' @export
`expr<-.quotation` <- function(q, value) {
  quo_(value, env(q))
}


#' @rdname quo
#' @export
#' @param x Any object.
is.quotation <- function(x) {
  inherits(x, "quotation")
}

#' `as.quo(x)` converts an object into a quotation. Closures,
#' formulas, and single-element [dots] can be converted this way.
#' @return `as.quo` returns a quotation.
#' @export
#' @rdname quo
as.quo <- function(x) {
  UseMethod("as.quo")
}

#' @export
#' @rdname quo
as.quo.quotation <- identity

#' @export
#' @rdname quo
as.quo.function <- function(x) {
  if (is.primitive(x)) stop("can't convert primitive to quotation")
  f <- formals(x)
  if (length(f) != 0) {
    stop("can only convert function to quotation if it has no args")
  }
  quo_(body(x), environment(x))
}

#' @export
#' @rdname quo
as.quo.quotation <- identity

#' @export
#' @rdname quo
as.quo.dots <- function(x) {
  if (length(x) == 1)
    x[[1]]
  else
    stop("can't convert nonscalar dots to a quotation")
}

#' @export
#' @rdname quo
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
#' @rdname quo
as.quo.formula <- function(x) {
  expr <- x[[2]]
  env <- attr(x, ".Environment")
  quo_(expr, env)
}

#' @export
#' @rdname quo
as.quo.lazy <- function(x) {
  quo_(x$expr, x$env)
}



