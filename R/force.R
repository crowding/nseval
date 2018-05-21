#' Forcing and forcedness of arguments and quotations.
#'
#' There are two kinds of [quotations](quo): forced and unforced.
#' Unforced quotations have an expression and an environment; forced
#' quotations have an expression and a value. `forced` returns a
#' logical or logical vector givenm a [quotation] or [dots].
#'
#' In normal R evaluation, function arguments are recorded as unforced
#' [promise]s, which are converted into forced promises as
#' well. Quotation objects therefore also exist in two varieties.
#' @export
#' @rdname forced
#' @param x A [quotation] or [dots] object.
#' @return `forced(x)` returns a [logical].
#' @seealso is_forced
forced <- function(x) UseMethod("forced")

#' The `forced` method on [dots] tests whether each element of a dots
#' list is forced, returning a named logical vector.
#' @export
#' @rdname dots
forced.dots <- function(x) {
  lapply(x, forced)
}

#' @export
#' @useDynLib nse _forced_quotation
#' @rdname forced
forced.quotation <- function(x, ...) {
  .Call(`_forced_quotation`, x)
}

#' @export
#' @rdname forced
forced.default <- function(x) forced(as.quo(x))

#' `forced_quo(x)` constructs a forced quotation that quotes its
#' argument literally.  It is equivalent to `quo(x,
#' force=TRUE)`.
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


#' `forced_dots` and `forced_dots_` create [dots] objects
#'   containing forced quotations.
#' @export
#' @rdname forced
#' @param ... any number of arguments; they will be quoted literally.
#' @return `forced_dots` and `forced_dots_` return [dots] objects.
#' @useDynLib nse _quotation_literal
forced_dots <- function(...) {
  list(...)
  get_dots(environment())
}

#' @param values A list; each element will be used as data.
#' @rdname dots
#' @export
forced_dots_ <- function(values) {
  structure(lapply(as.list(values),
                   function(x) .Call(`_quotation_literal`, x)),
            class="dots")
}

#' `force_(x)` evaluates the contents of a quotation or dots object.
#' if they are not already forced, and returns a new forced object.
#' @export
#' @rdname forced
#' @seealso force
force_ <- function(x, ...) {
  UseMethod("force_")
}

#' @export
#' @rdname forced
#' @param eval Call something other than 'eval' to "evaluate" the expression.
force_.quotation <- function(x, eval=base::eval, ...) {
  if (forced(x)) {
    q
  } else {
    .Call('_quotation', NULL, expr(x), eval(expr(x), env(x)))
  }
}

#' @export
#' @rdname forced
force_.dots <- function(x, ...) {
  structure(lapply(x, force_.quotation, ...), class="dots")
}

#' `value` or `values` returns the value of a quotation or dots,
#'   forcing it if necessary.
#' @rdname forced
#' @return `value` returns the result of forcing the quotation.
#' @export
value <- function(x, ...) {
  UseMethod("value")
}

#' @export
#' @rdname forced
#' @param mode Whether to force in "any" mode or "function" mode (see
#'   [locate]).
value.quotation <- function(x, mode="any", ...) {
  if (forced(x)) {
    x()
  } else {
    switch(mode,
           "any" = eval(body(x), environment(x)),
           "function" = stop("Not implemented"),
           stop("Invalid mode")
           )
  }
}

#' @export
#' @rdname forced
value.dots <- function(x, ...) {
  do(list, x)
}

##' @export
##' @rdname forced
#value.default <- function(f) value(as.quo(f))

#' @rdname forced
#' @return `values` returns a list.
#' @export
values <- function(x) {
  UseMethod("values")
}

#' @rdname forced
#' @export
values.dots <- function(x) {
  do(list, x)
}

## #' @rdname forced
## #' @export
## values.default <- function(d) {
##   do(list, as.dots(d))
## }
