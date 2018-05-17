#' Forcing and forcedness of arguments and quotations.
#'
#' There are two kinds of [quotations](quo): forced and unforced.
#' Unforced quotations have an expression and an environment; forced
#' quotations have an expression and a value. `forced` returns a
#' logical or logical vector givenm a [quotation] or [dots].
#'
#' In normal R evaluation, function arguments are recorded as unforced
#' [promise]s, which are converted into forced promises as
#' well. Quotation objects therefore exist in the same two varieties.
#'
#' @export
#' @rdname forced
#' @return `forced(x)` returns a [logical] value.
#' @seealso is_forced
forced <- function(x) UseMethod("forced")


#' `forced_quo(x)` constructs a forced quotation directly from its
#' argument.  It is equivalent to `quo(x, force=TRUE)`.
#' @rdname forced
#' @export
forced_quo <- function(x) {
  force(x)
  arg(x)
}

#' `forced_quo_(x)` makes a forced quotation from any data.
#' Specifically it constructs a [quotation] with the same object in
#' both the `expr` and `value` slots, except if is a
#' [language](is.language) object in which case the value is wrapped
#' in `quote()`.
#' @rdname forced
#' @return `forced_quo` and `forced_quo_` return [quotation](quo)
#'   objects.
forced_quo_ <- function(x) {
  .Call(`_quotation_literal`, x)
}


#' `forced_dots` and `forced_dots_` construct a [dots] object
#'   containing forced quotations.
#' @rdname forced
#' @export
#' @return `forced_dots` and `forced_dots_` return [dots] objects.
#' @useDynLib nse _quotation_literal
#' @param ... any number of arguments.
forced_dots <- function(...) {
  list(...)
  get_dots(environment())
}

#' `forced_dots`
#' @export
forced_dots_ <- function(values) {
  structure(lapply(as.list(values),
                   function(x) .Call(`_quotation_literal`, x)),
            class="dots")
}

# hmm:  force_(quo(x)) is not equivalent to force(x) because it returns a quo.

#' `force_(x)` evaluates the contents of a quotation or dots object.
#' if they are not already forced, and returns a new forced object.
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

#' `value` or `values` returns the value of a quotation or dots,
#'   forcing it if necessary.
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
#' @rdname forced
value.dots <- function(d) {
  do(list, d)
}

##' @export
##' @rdname forced
#value.default <- function(f) value(as.quo(f))

#' @rdname forced
#' @return `values` returns a list.
#' @export
values <- function(p) {
  UseMethod("values")
}

#' @rdname forced
#' @export
values.dots <- function(d) {
  do(list, d)
}

## #' @rdname forced
## #' @export
## values.default <- function(d) {
##   do(list, as.dots(d))
## }
