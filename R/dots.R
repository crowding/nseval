#' Show information about a \dots object.
#'
#' `unpack` transforms the contents of a \dots object, into a data
#' frame with columns "name", "expr", "envir" and "value".
#'
#' @param ... Any number of arguments. Usually, you will pass in the
#' ... from the body of a function,
#' e.g. \code{dots_unpack(...)}.
#'
#' @note There are some issues with R printing data frames containing
#'   lists of language objects (and more problems when working with
#'   "missing value" objects.) Therefore this sets the class on the
#'   columns to one that has a special as.character method.
#' @seealso dots_names dots_missing dots_exprs dots
#' @aliases unpack
#' @export
dots_unpack <- function(...) {
  unpack(dots(...))
}

#' @export
#' @rdname dots_unpack
#' @return A data frame, with one row for each element of \code{\...},
#'   and columns: \describe{ \item{"name"}{The name of each argument,
#'   or "" if present.}  \item{"envir"}{The enviroment the promise
#'   came from.}  \item{"expr"}{The expression attached to the
#'   promise. If the promise has been evaluated, this will be NULL.}
#'   \item{"value"}{The value attached to the promise. If the promise
#'   has not been evaluated, this will be NULL.}}

#' @param x A \code{\link{dots}} object.
unpack <- function(x) UseMethod("unpack")

#' @export
#' @useDynLib nse _dots_unpack
unpack.dots <- function  (x) {
  x <- .Call(`_dots_unpack`, x)
  class(x$envir) <- "oneline"
  class(x$expr) <- "oneline"
  class(x$value) <- "oneline"
  attr(x, "row.names") <- make.unique(x$name)
  x
}

#' \code{exprs(dots(...))} extracts a list of expressions, one per element
#' of a \code{\link{dots}} object.
#'
#' @param x A dots object (see \code{\link{dots}}).
#' @return A named list of expressions. The mutator \code{exprs<-}
#'   constructs a new dots object with the new expressions.
#' @rdname dots
#' @export
exprs <- function(x) UseMethod("exprs")

#' @export
#' @rdname dots
exprs.dots <- function(x) {
  lapply(unclass(x), expr)
}

#' @export
#' @rdname dots
#' @return `dots_exprs(...)` is a shorthand for exprs(dots(...))
#' @useDynLib nse _dots_exprs
#' @useDynLib nse _get_dots
dots_exprs <- function(...) {
  .Call("_dots_exprs", .Call("_get_dots", environment()))
}

#' @export
#' @param value A list of expressions.
#' @rdname dots
`exprs<-` <- function(x, value) {
  UseMethod("exprs<-")
}

#' @export
`exprs<-.dots` <- function(x, value) {
  structure(mapply(FUN=quo_, SIMPLIFY=FALSE,
                   expr = value, env = envs(x)), class="dots")
}

#' @rdname dots
#' @return \code{envs(dots(...)} extracts a list of environments from a
#' \code(link(dots)) object.
#' @export
envs <- function(x) {
  UseMethod("envs")
}

#' @rdname dots
#' @return \code{envs(x)} returns a list of the environments of each
#'   quotation in x.
#' @param x a `[dots]` object.
#' @export
envs.dots <- function(x) {
  lapply(x, environment)
}

#' @rdname dots
#' @return `envs<-` returns a list of new quotations with updated
#'   environments.
#' @param value A replacement list.
#' @export
`envs<-` <- function(x, value) {
  UseMethod("envs<-")
}

#' @export
#' @rdname dots
#' @return `dots_exprs(...)` is a shorthand for exprs(dots(...))
#' @useDynLib nse _dots_envs
#' @useDynLib nse _get_dots
dots_envs <- function(...) {
  .Call("_dots_envs", .Call("_get_dots", environment()))
}

#' @export
#' @rdname dots
#' @return `dots_names(...)` is a shorthand for names(dots(...))
#' @useDynLib nse _dots_names
#' @useDynLib nse _get_dots
dots_names <- function(...) {
  .Call("_dots_names", .Call("_get_dots", environment()))
}


#' @rdname dots
#' @export
#' @return `envs<-` returns a list of new quotations with updated
#'   expressions.
`envs<-.dots` <- function(x, value) {
  structure(mapply(FUN=quo_,
                   expr=exprs(x),
                   env=value,
                   SIMPLIFY = FALSE),
            class="dots")
}

#' @export
#' @rdname missing_
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



#' Capture a list of arguments as an object.
#'
#' A dots object represents a named list of quotations. It mirrors R's
#' special variable `...`. Unlike `...`, a `dots` is an immutable
#' (evaluating does not change it), first-class (you can store it in
#' any variable, not just `...`) data object (The R interpreter passes
#' it along like data rather than triggering argument splicing).
#'
#' \code{d <- dots(...)} captures the contents of ... without
#' triggering evaluation, and returns a list of class "dots", each
#' element of which is a \code{\link{quotation}}. This extends,
#' e.g. \code{substitute(list(...))[[2]]} by capturing the context of
#' each expression along with expressions.
#'
#' \code{d <- dots(foo, quux=bar+baz)} captures all of its arguments
#' in a dots object, like \code{\link{alist}()}, but also captures the
#' environment of each argument.)
#'
#' @param ... Any number of arguments.
#' @return A list with class 'dots', each element of which is a [quotation].
#' @seealso \%()\%
#' @examples
#'
#' named.list <- function(...) {
#'  # Collect only named arguments, ignoring unnamed arguments.
#'  d <- dots(...)
#'  list %()% d[names(d) != ""]
#' }
#' named.list(a=1, b=2*2, stop("this is not evaluated"))
#' @export
dots <- function(...) {
  get_dots(environment())
}

#' @export
`[.dots` <- function(x, ..., drop=FALSE) {
  y <- NextMethod("[")
  structure(y, class="dots")
}

#' @export
`[<-.dots` <- function(x, ..., value)
{
  if (!is.null(value)) value <- as.dots(value)
  y <- NextMethod("[")
  structure(y, class="dots")
}

#' @export
as.dots.promise <- function(x) {
  structure(list(x), class="dots")
}


#' \code(dots_) is the standard-evaluating constructor for a dots
#' object.
#'
#' @rdname dots
#' @param ... promise objects, dots objects, lists of things and things
#' @seealso promise
#' @param exprs An expression or list of expressions (if
#'   \code{promises} is not given)
#' @param envs An environment or list of environments (if \code{promises}
#'   is not given)
dots_ <- function(..., exprs, envs) {
  if (missing(promises)) {
    if (!is.list(exprs)) {
      exprs <- list(exprs)
    }
    if (!is.list(envs)) {
      envs <- list(envs)
    }
    structure(mapply(FUN=quo_, exprs, envs, SIMPLIFY=FALSE), class="dots")
  } else {
    as.dots(promises)
  }
}

#' R's missing value.
#'
#' The missing constant (`R_MissingArg` at C level) has two related
#' uses. One is "at parse time" when it is used to represent empty
#' arguments. The other is "at run time" when it is bound to function
#' arguments that were not given any value.
#'
#' Manipulating expressions ("computing on the language") means we
#' have to deal with the first use case, because we have to be able to
#' make expressions that have empty arguments, like the first index in
#' \code{arr[,c]}.
#'
#' The second use of the missing sigil makes this tricky. Generally it
#' is a bad idea to assign a bare `missing_value` to a variable or use
#' one as the argument to a function, because this makes R think that
#' the variable \b{is} missing rather than that it \b{contains a}
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
#' with missing_values, then, best to keep them wrapped up in lists or
#' other objects.
#'
#' @param n Optional; a number. If provided, will return a list of
#' missing values with this many elements.
#' @return A symbol with empty name, or a list of such.
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

#' Apply a list of arguments to a function.
#'
#' These operators help in passing arbitrary lists of arguments to
#' functions, with a more flexiple interface than
#' \code{\link{do.call}}.
#'
#' @param arglist A dots object, or something that can be converted
#'   into a dots object.
#' @param f a function, to be called, or to to have arguments attached
#'   to.
#' @return The result of calling the function with the arguments
#'   provided. When \code{x} is a \code{\dots} object, its contents
#'   are passed inithout evaluating. When \code{x} is another type of
#'   sequence its elements are put in the value slots of
#'   already-evaluated promises. This is slightly different behavior
#'   from \code{\link{do.call}(f, as.list(x), quote=TRUE)}, which
#'   passes unevaluated promises with expressions wrapped in
#'   \code{link{quote}}. This makes a difference if \code{f} performs
#'   nonstandard evaluation.
#' @rdname call
#'
#' @export "%()%"
`%()%` <- function(f, arglist)
    UseMethod("%()%", arglist)

#' @export
`%()%.dots` <- function(f, arglist) {
  if (length(arglist) == 0) return(f())
  (function(...) {
    set_dots(environment(), arglist)
    f(...)
  })()
}

#' Set the binding of "..." in an environment.
#'
#' If an empty dotlist is given, the "..." binding is set to the
#' missing value.
#'
#' @param env The environment to update.
#' @param dots a \code{\link{dots}} object.
#' @param append Whether to append to an existing binding for "..." if
#'   the environment has one.
#' @return the updated environment, invisibly.
#' @export
#' @useDynLib nse _set_dots
#' @useDynLib nse _flist_to_dotsxp
set_dots <- function(env, dots, append=FALSE) {
  if (append) {
    dots = c(get_dots(env), dots);
  }
  .Call(`_set_dots`, .Call(`_flist_to_dotsxp`, dots), env)
  invisible(env)
}

#' Inspect "..." in some environment to make sure that If there is no
#' "..." in the given environment, or it is missing, returns an empty
#' dotslist.
#'
#' @param env The environment to look in.
#' @return the contents of `...` converted to a `dots` object.
#' @export
#' @useDynLib nse _get_dots
#' @useDynLib nse _dotsxp_to_flist
get_dots <- function(env = caller(environment())) {
  dts <- .Call(`_get_dots`, env)
  .Call(`_dotsxp_to_flist`, dts)
}

#' @export
`%()%.default` <- function(f, arglist) {
  #if (length(arglist) == 0) return(f())
  set_dots(environment(), as.dots.literal(arglist))
  f(...)
}

#' Check for blank expression.
#'
#' For [`dots`] and [`quo`] objects, checks whether the expressions are missing
#' without evaluating.
#'
#' For lists, check if they are identical to R's "missing value"
#'
#' @details
#' Checking for missing elements of `...`, without forcing, can be
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
#' and \link{is_missing}(x) to detect missing arguments.
#'
#' @param x
#' @return a vector of boolean values
#' @seealso missing, is_missing
#' @export
missing_ <- function(x) if (missing(x)) TRUE else UseMethod("missing_")

#' @export
missing_.dots <- function(x) {
  vapply(exprs(x), identical, FALSE, missing_value())
}

#' @export
missing_.default <- function(x) {
  if (identical(x, missing_value()))
    TRUE
  else if (is.list(x))
    vapply(x, identical, FALSE, quote(expr=))
  else
    rep(FALSE, length(x))
}

#' @export
missing_.quotation <- function(x) {
  identical(expr(x), missing_value())
}

#FIXME: this should not be called "is"

#' Evaluate promises or dots.
#'
#' For promise objects, evaluates the given promise and returns the
#' result. For "dots" objects, evaluates each element and returns a
#' list of results. Unlike native R promises, the results will not be
#' retained.
#'
#' @param p a promise object.
#' @param d a dots object.
#' @return a value or list of values.
#' @export
value <- function(p) {
  UseMethod("value")
}

values <- function(p) {
  UseMethod("values")
}

#' @export
values.dots <- function(d) {
  list %()% d
}


## Local Variables:
## ess-r-package-info: ("nse" . "~/fexpr")
## End:
