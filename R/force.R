#' Working with forced quotations and dots.
#'
#' In R's implementation of lazy evaluation, function arguments are
#' first recorded as a `promise` which pair of an expression with a
#' source environment. After a promise's value is needed, it is
#' converted to a "forced" promise, which records the expression and
#' value, but no longer records the source environment (this allows
#' the environments to be garbage collected.) [Quotation](quo) objects
#' also exist in unforced and forced varieties.
#'
#' @export
#' @rdname forced
#' @return `forced(x)` returns a [logical] value.
#' @seealso is_forced
forced <- function(x) UseMethod("forced")


#' (forced)
#'
#' `forced_quo(x)` forces its argument before capturing it as a
#' quotation (including the literal expression). It is equivalent to
#' `quo(x, force=TRUE)`.
#' @rdname forced
#' @return `forced_quo` and `forced_quo_` return [quotation](quo) objects.
#' @export
forced_quo <- function(x) {
  force(x)
  arg(x)
}

#' (forced)
#'
#' `forced_quo_(x)` makes a forced quotation from any data.
#' Specifically it constructs a [quotation] with the same
#' object in both the `expr` and `value` slots, and does not capture
#' the expression like `forced_quo`.
#' @rdname forced
forced_quo_ <- function(x) {
  .Call(`_quotation_literal`, x)
}


#' (forced)
#'
#' `forced_dots` and `forced_dots_` construct a [dots] object
#'   containing forced quotations, analogously to `forced_quo` and
#'   `forced_quo_`.
#' @rdname forced
#' @export
#' @return `forced_dots` and `forced_dots_` return [dots] objects.
#' @useDynLib nse _quotation_literal
#' @param ... any number of arguments.
forced_dots <- function(...) {
  list(...)
  get_dots(environment())
}

#' @rdname forced
#' @export
forced_dots_ <- function(values) {
  structure(lapply(as.list(values),
                   function(x) .Call(`_quotation_literal`, x)),
            class="dots")
}



#' (forced)
#'
#' `force_(x)` evaluates the contents of a quotation or dots object,
#' if they are not already forced, and returns a new, forced quotation
#' or dots object.
#' @export
#' @rdname forced
#' @seealso force
force_ <- function(q, ...) {
  UseMethod("force_")
}

#' @export
#' @rdname forced
#' @param eval Call something other than 'eval' to "evaluate" the expression.
force_.quotation <- function(q, eval=base::eval) {
  if (forced(q)) {
    q
  } else {
    .Call('_quotation', NULL, expr(q), eval(expr(q), env(q)))
  }
}

#' @export
#' @rdname forced
force_.dots <- function(d, ...) {
  structure(lapply(d, force_.quotation, ...), class="dots")
}


#' @export
#' @useDynLib nse _forced_quotation
#' @rdname forced
forced.quotation <- function(x) {
  .Call(`_forced_quotation`, x)
}

#' @export
#' @rdname forced
forced.default <- function(x) forced(as.quo(x))

#' (forced)
#'
#' `value` or `values` returns the value of a quotation, forcing it if
#'   necessary.
#' @param q a [quotation] or [dots] object.
#' @rdname forced
#' @return `value` returns the result of forcing the quotation.
#' @export
value <- function(q, ...) {
  UseMethod("value")
}

#' @export
#' @rdname forced
#' @param mode Whether to force in "any" mode or "function" mode (see
#'   [locate]).
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

#' @export
value.dots <- function(d) {
  do(list, d)
}

##' @export
#value.default <- function(f) value(as.quo(f))

#' @rdname forced
#' @return `values` returns a list.
#' @export
values <- function(p) {
  UseMethod("values")
}

#' @export
values.dots <- function(d) {
  do(list, d)
}

#' @export
values.default <- function(d) {
  do(list, as.dots(d))
}
