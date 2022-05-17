#' Forcing and forcedness of arguments and quotations.
#'
#' There are two kinds of [quotation]s: forced and unforced.
#' Unforced quotations have an expression and an environment; forced
#' quotations have an expression and a value.
#'
#' @export
#' @rdname forced
#' @param x A [quotation] or [dots] object.
#' @return `forced(x)` returns a [logical].
#' @seealso is_forced forced_quo
forced <- function(x) UseMethod("forced")

#' @rdname forced
#' @description
#' `forced(q)` tests whether a [quotation] is forced.
#' @export
#' @useDynLib nseval _forced_quotation
forced.quotation <- function(x, ...) {
  .Call("_forced_quotation", x)
}

#' @rdname forced
#' @description
#' `forced(d)` on a [dots] object tests whether each element
#' is forced, and returns a logical vector.
#' @export
forced.dots <- function(x) {
  lapply(x, forced)
}

#' @export
#' @rdname forced
forced.default <- function(x) forced(as.quo(x))

#' @rdname quo
#' @description
#' `forced_quo(x)` captures the expression in its argument, then
#' forces it, returning a quotation with the expression and value.
#' @export
forced_quo <- function(x) {
  force(x)
  arg(x)
}

#' @rdname quo
#' @description
#' `forced_quo_(val)` makes a [forced] quotation given a value.
#' Specifically it constructs a [quotation] with the same object in
#' both the `expr` and `value` slots, except if is a
#' [language](is.language) object in which case the `expr` slot is wrapped
#' in `quote()`.
#' @param val A value.
forced_quo_ <- function(val) {
  .Call("_quotation_literal", val)
}


#' @rdname dots
#' @description
#' `forced_dots(...)` forces its arguments and constructs a `dots` object with
#' [forced] quotations.
#' @export
#' @useDynLib nseval _quotation_literal
forced_dots <- function(...) {
  list(...)
  get_dots(environment())
}

#' @rdname dots
#' @description
#' `forced_dots_(values)` creates a dots object from a list of values
#' @param values A list; each element will be used as data.
#' @export
forced_dots_ <- function(values) {
  structure(lapply(as.list(values),
                   function(x) .Call("_quotation_literal", x)),
            class="dots")
}


#' @rdname forced
#' @description
#' `force_(x)` converts an unforced quotation or dots object into a
#' forced one, by evaluating it.
#' @export
#' @param ... Options used by methods
#' @seealso [force]
force_ <- function(x, ...) {
  UseMethod("force_")
}

#' @export
#' @rdname forced
#' @param eval Which evaluation function to use.
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

#' @rdname forced
#' @description
#' `value(x)` or `values(...)` returns the value of a quotation or dots,
#'   forcing it if necessary.
#' @return `value(x)` returns the result of forcing the quotation.
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

# #' @export
# #' @rdname forced
# value.default <- function(f) value(as.quo(f))

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
