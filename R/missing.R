#' R's missing value.
#'
#' `missing_value()` returns R's missing object; what R uses to
#' represent a missing argument. It is distinct from either [NULL] or
#' [NA].
#'
#' The missing value occurs naturally in a quoted R expression that has an empty argument:
#' \preformatted{  exp <- quote( x[1, ] )
#'   identical(exp[[4]], missing_value()) #TRUE
#'   is_missing(exp[[4]]) #also TRUE
#' }
#' So we can use `missing_value()` to help construct expressions:
#' \preformatted{  substitute(f[x, y], list(x = 1, y=missing_value()))
#' }
#' When such an expression is evaluated and starts a function call,
#' the missing value winds up in the promise expression.
#' \preformatted{  f <- function(x) arg_expr(x)
#'   identical(f(), missing_value()) # TRUE
#' }
#' During "normal evaluation", finding a missing value in a
#' variable raises an error.
#' \preformatted{  m <- missing_value()
#'   list(m) # raises error
#' }
#' This means that it's sometimes tricky to work with missings:
#' \preformatted{  exp <- quote( x[1, ] )
#'   cols <- x[[4]]
#'   x <- list(missing_value(), 2, 3)     # this is ok, but...
#'   a <- missing_value(); b <- 2; c <- 3 # this stores missing in "cols",
#'   x <- list(a, b, c)                   # throws an error: "a" missing
#' }
#' Generally, keep your missing values wrapped up in lists or quotations,
#' instead of assigning them to variables directly.
#'
#' @param n Optional; a number. If provided, will return a list of
#'   missing values with this many elements.
#' @return `missing_value` returns the symbol with empty name, or a
#'   list of such.
#' @seealso missing is_missing
#' @examples
#' # These expressions are equivalent:
#' function(x, y=1) {x+y}
#' function_(list(x=missing_value, y=1),
#'           quote( {x+y} ))
#'
#' # These expressions are also equivalent:
#' quote(df[,1])
#' substitute(df[row,col],
#'            list(row = missing_value(), col = 1))
#' @export
missing_value <- function(n) {
  if (missing(n)) {
    quote(expr=)
  } else {
    rep(list(quote(expr=)), n)
  }
}

#' `missing_` compares expressions with the missing value. It is a generic
#' function with methods for [dots], [quotation]s and lists.
#' @param x a value, [dots], or list.
#' @param unwrap Whether to descend recursively through unevaluated
#'   promises using `unwrap(x, TRUE)`
#' @return `missing_` returns a logical vector.
#' @seealso missing is_missing
#' @rdname missing_value
#' @export
missing_ <- function(x, unwrap=TRUE) {
  if (missing(x)) TRUE
  else UseMethod("missing_")
}

#' `missing_` on a list compares each element of the list to the
#' missing value, and returns a logical vector.
#' @rdname missing_value
#' @export
missing_.default <- function(x, unwrap=TRUE) {
  if (identical(x, missing_value()))
    TRUE
  else if (is.list(x))
    vapply(x, identical, FALSE, quote(expr=))
  else
    rep(FALSE, length(x))
}

#' `missing_` on [dots] and [quotation] objects checks whether the
#' expressions are identical to the missing value.
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
missing_.quotation <- function(x, unwrap=TRUE) {
  if (unwrap)
    x <- unwrap(x, TRUE)
  identical(expr(x), missing_value())
}

#' `list_missing` is similar to `list` but allows missing arguments.
#' @rdname missing_value
#' @param ... Arguments evaluated normally. except those which are missing.
#' @return `list_missing` returns a list.
#' @examples
#' # How to do the trick of `[` where it can tell which arguments are
#' # missing:
#' `[.myclass` <- function(x, ...) {
#'    indices <- list_missing(...)
#'    kept.axes <- which(missing_(indices))
#'    cat(paste0("Keeping axes ", kept_axes, "\n"))
#'    #...
#' }
#' ar <- structure(array(1:24, c(2, 3, 4)))
#' ar[, 3, ]
#' @export
list_missing <- function(...) {
  lapply(dots(...), function(x) {
    if(missing_(x))
      missing_value()
    else
      value(x)
  })
}
