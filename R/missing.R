#' R's missing value.
#'
#' The missing value (`R_MissingArg` at C level) has two related uses
#' in R's implementation. One is "at parse time" when it is used to
#' represent empty arguments. The other is "at run time" when it is
#' bound to function arguments that were not given any value.
#'
#' Manipulating expressions ("computing on the language") means we
#' have to deal with the first use case, because we have to be able to
#' make calls that have empty arguments, like the first index in
#' `arr[,c]`.
#'
#' The second use of the missing sigil makes this tricky. Generally it
#' is a bad idea to assign a bare `missing_value` to a variable or use
#' one as the argument to a function, because this makes R think that
#' the variable *is* missing rather than that it *contains a*
#' missing. For instance, you can say
#'
#'     x <- list(missing_value(), 2, 3)
#'
#' and get a valid list, but this:
#'
#'     a <- missing_value(); b <- 2; c <- 3
#'     x <- list(a, b, c)
#'
#' fails with an error about the missing variable "a". When dealing
#' with missing values, then, best to keep them wrapped up in lists,
#' [quotations] or others
#'
#' @param n Optional; a number. If provided, will return a list of
#' missing values with this many elements.
#' @return The symbol with empty name, or a list of such.
#' @seealso list_missing
#' @examples
#' # These expressions are equivalent:
#' quote(function(x, y=1) x+y)
#' call("function", pairlist(x=missing_value(), y=1), call("+", as.name("x"), as.name("y"))
#'
#' # These expressions are also equivalent:
#' quote(df[,1])
#' substitute(df[row,col], list(row = missing_value(), col = 1))
#' @export
missing_value <- function(n) {
  if (missing(n)) {
    quote(expr=)
  } else {
    rep(list(quote(expr=)), n)
  }
}


#' @export
#' @rdname missing_value
#' @return `list_missing` A list containing the values of all arguments, including
#'   missing values. That is, \code{list_missing} works like
#'   \code{list}, but does not complain about missing arguments,
#'   instead representing them directly.
list_missing <- function(...) {
  lapply(dots(...), function(x) {
    if(missing_(x))
      missing_value()
    else
      value(x)
  })
}


#' ...
#'
#' `missing_` checks for missingness in R data.
#'
#' @details
#' Checking for missing arguments of `...`, without forcing, can be
#' useful if you need to implement array subsetting like \code{`[`},
#' where a missing argument means to take all indexes on that
#' dimension.
#'
#' There is not a good way to emulate \code{`[`}'s behavior in base R;
#' using `list(\dots)` to collect all positional arguments will throw
#' errors on missing arguments. Meanwile, using
#' `substitute(list(...))[[2]]` gives you the unevaluated arguments,
#' but stripts them of their environments (breaking hygeine).
#'
#' Instead, use \code{x <- list_missing(...)}
#' and \link{missing_}(x) to detect missing arguments.
#'
#' @param x a value, [dots], or list.
#' @param unwrap Whether to descend through unevaluated promises
#'   using [unwrap(x, TRUE)] before deciding if a promise is missing.
#' @return `missing` returns a logical vector
#' @seealso missing, is_missing
#' @rdname missing_value
#' @export
missing_ <- function(x, unwrap=TRUE) {
  if (missing(x)) TRUE
  else UseMethod("missing_")
}

#' ...
#'
#' For `[dots]` and `[quo]` objects, `missing()` checks whether the
#' expressions are missing without evaluating.
#' @rdname missing_value
#' @export
missing_.dots <- function(x, unwrap=TRUE) {
  if (unwrap) {
    x <- unwrap(x, TRUE)
  }
  vapply(exprs(x), identical, FALSE, missing_value())
}

#' @export
#' @rdname missing_value
missing_.default <- function(x, unwrap=TRUE) {
  if (identical(x, missing_value()))
    TRUE
  else if (is.list(x))
    vapply(x, identical, FALSE, quote(expr=))
  else
    rep(FALSE, length(x))
}

#' @export
#' @rdname missing_value
missing_.quotation <- function(x, unwrap=TRUE) {
  if (unwrap)
    x <- unwrap(x, TRUE)
  identical(expr(x), missing_value())
}
