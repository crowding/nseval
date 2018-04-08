
#' Capture or construct a quotation.
#'
#' A quo (or quotation) `q <- quo( <anything> )` is an object with two
#' parts: an expression [expr(q)] with an environment [env(q)]. (Like
#' in writing, an 'expression' may be a phrase used in many different
#' contexts, but a 'quotation' needs to be bundled with a citation, to
#' reference a context in which it was said.)
#'
#' A quo is parallel to a 'promise' which is the data structure R uses
#' to hold lazily evaluated arguments. A quo is different from a promise
#' because it is an immutable data object.
#'
#' As a data object, a quo does not automatically evaluate like a
#' promise, but can be evaluated explicitly with the method
#' `value(q)`.  A quo is immutable, so it does not mutate into a
#' "forced" state if you choose to evaluate it.
#'
#' Quotation objects can be extracted from the arguments to a currently
#' evaluating function using [arg].
#'
#' A [dots] object is a list of quotations.
#'
#' @export
#' @param x an argument, left unevaluated.
#' @return an object of class "quotation".
#' @param force Create a "forced" quotation; this is one that stores
#'   an expression and value but no environment.
#' @rdname quo
quo <- function(x, env = arg_env_(quote(x), environment()), force = FALSE) {
  quo_(arg_expr_(quote(x), environment()), env = env, force = force)
}

#' quo_ is a normally evaluating, explicit constructor for quotations.
#' @rdname quo
#' @param expr An expression.
#' @param env An environment.g
#' @export
#' @useDynLib nse _quotation
quo_ <- function(expr, env, force=FALSE) {
  if(force) {
    .Call(`_quotation`, NULL, expr, eval(expr, env));
  } else {
    .Call(`_quotation`, env, expr, NULL);
  }
}

#' @rdname quo
#' @export
env <- function(x) UseMethod("env")

#' @rdname quo
#' @export
env.quotation <- function(x) {
  environment(x)
}

#' @rdname quo
#' @export
`env<-` <- function(x, value) {
  UseMethod("env<-")
}

#' @rdname quo
#' @export
`env<-.quotation` <- function(x, value) {
  quo_(expr(x), value);
}

#' @rdname quo
#' @export
expr <- function(q) UseMethod("expr")

#' @rdname quo
#' @export
#' @return `expr(q)` accesses the expression.
#' @param q A quotation object.
expr.quotation <- function(q) {
  .Call("_expr_quotation", q)
}

#' @rdname quo
#' @export
`expr<-` <- function(x, value) {
  UseMethod("expr<-")
}

#' @rdname quo
#' @export
`expr<-.quotation` <- function(x, value) {
  quo_(value, env(x))
}

#' @export
force_ <- function(q, ...) {
  UseMethod("force_")
}

#' @export
force_.quotation <- function(q, eval=base::eval) {
  if (forced(q)) {
    q
  } else {
    .Call('_quotation', NULL, expr(q), eval(expr(q), env(q)))
  }
}

#' @rdname quo
#' @return `value(q)` evaluates a quotation. If the quotation is
#'   already [forced](is_forced) then evaluation retreives the stored
#'   value and has no other effect. `value()` does not change the
#'   quotation to forced; repeated calls to `value` will have repeated
#'   effects.
#' @export
value.quotation <- function(q, mode="any") {
  if (forced(q)) {
    q()
  } else {
    switch(mode,
           "any" = eval(body(q), environment(q)),
           "function" = stop("Not implemented"),
           stop("Invalid mode")
           )
  }
}

##' @export
#value.default <- function(f) value(as.quo(f))

#' @export
forced <- function(x) UseMethod("forced")

#' @export
#' @useDynLib nse _forced_quotation
forced.quotation <- function(x) {
  .Call(`_forced_quotation`, x)
}

##' @export
#forced.default <- function(x) forced(as.quo(x))

#' @export
#' @return `as.quotation.literal(x)` creates a forced promise, containing `x`
#'    in both the expression and data slots.
as.quo.literal <- function(x) {
  .Call(`_quotation_literal`, x)
}

#' @rdname quo
#' @export
is.quotation <- function(x) {
  inherits(x, "quotation")
}
