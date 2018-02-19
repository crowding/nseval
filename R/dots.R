#' Show information about a \dots object.
#'
#' This unpacks the contents of a \dots object, returning the results
#' in a data frame. In the R implementation, a \dots object is a list
#' of promises , usually bound to the special name \code{"..."} and,
#' when bound to that name, given special dispensation by the R
#' interpreter when appearing in the argument list of a call. Dots
#' objects are normally opaque to R code, and usually don't appear as
#' first-class objects in user code, though you can obtain one
#' object in base R by using \code{get("...")}.
#'
#' @param ... Any number of arguments. Usually, you will pass in the
#' ... from the body of a function,
#' e.g. \code{dots_unpack(...)}.
#'
#' @return A data frame, with one row for each element of
#' \code{\...}, and columns: \describe{ \item{"name"}{The name of
#' each argument, or "" if present.}  \item{"envir"}{The enviroment
#' the promise came from.}  \item{"expr"}{The expression attached to
#' the promise. If the promise has been evaluated, this will be NULL.}
#' \item{"value"}{The value attached to the promise. If the promise
#' has not been evaluated, this will be NULL.}}
#' @note There are some problems with R printing data frames
#' containing lists of language objects (and more problems when
#' working with "missing value" objects.) Therefore this sets the
#' class on the columns to one that has a special as.character method.
#' @seealso dots_names dots_missing dots_exprs dots
#' @aliases unpack
#' @useDynLib promises _dots_unpack
#' @export
dots_unpack <- function(...) {
  unpack(dots(...))
}

#' @export
#' @rdname dots_unpack
#' @param x A \code{\link{dots}} object.
unpack <- function(x) UseMethod("unpack")

#' @export
#' @useDynLib promises _dots_unpack
unpack.dots <- function  (x) {
  x <- .Call(`_dots_unpack`, x)
  class(x$envir) <- "oneline"
  class(x$expr) <- "oneline"
  class(x$value) <- "oneline"
  attr(x, "row.names") <- make.unique(x$name)
  x
}

#' Extract unevaluated expressions.
#'
#' \code{exprs(dots(...))} extracts a list of expressions, one per element
#' of a \code{\link{dots}} object.
#'
#' @param x A dots object (see \code{\link{dots}}).
#' @return A named list of expressions. The mutator \code{exprs<-}
#'   constructs a new dots object with the new expressions.
#' @seealso
#' @rdname
#' @export
exprs <- function(x) UseMethod("exprs")

#' @export
#' @rdname dots_exprs
#' @useDynLib promises _get_expr
exprs.dots <- function(x) {
  lapply(unclass(x), body)
}

#' @export
#' @rdname dots_exprs
#' @return A list containing the unevaluated
#' expressions of each argument.
list_quote <- function(...) {
  exprs(dots(...))
}

#' @export
#' @param value A list of expressions.
#' @rdname dots_exprs
`exprs<-` <- function(x, value) {
  UseMethod("exprs<-")
}

#' @export
`exprs<-.dots` <- function(x, value) {
  structure(mapply(FUN=promise_, SIMPLIFY=FALSE,
                   expr = value, env = envs(x)), class="dots")
}

#' Extract or update environments contained in dots lists.
#'
#' \code{envs(dots(...)} extracts a list of environments from a
#' \code(link(dots)) object.
#'
#' @param x a \{code{\link{dots}} object.
#' @return A named list of environment objects. The mutator
#'   \code{envs<-} constructs a new list of unevaluated promises with
#'   the same expressions but different environments.
#' @export
envs <- function(x) {
  UseMethod("envs")
}

#' @export
envs.dots <- function(x) {
  lapply(x, environment)
}

#' @param value An environment or list of environments.
#' @rdname envs
#' @export
`envs<-` <- function(x, value) {
  UseMethod("envs<-")
}

#' @export
`envs<-.dots` <- function(x, value) {
  structure(mapply(FUN=promise_,
                   expr=exprs(x), env=value,
                   SIMPLIFY = FALSE),
            class="dots")
}

#' @export
#' @rdname is.missing
#' @return A list containing the values of all arguments, including
#'   missing values. That is, \code{list_missing} works like
#'   \code{list}, but does not complain about missing arguments,
#'   instead representing them directly.
list_missing <- function(...) {
  lapply(dots(...), function(x) {
    if(is.missing(x))
      missing_value()
    else
      value(x)
  })
}


#' Capture a list of arguments as an object.
#'
#' A dots object represents a named list of unevaluated arguments. It
#' is a representation of the special symbol `...` that appears in R
#' code.
#'
#' \code{d <- dots(...)} captures the contents of ... without
#' evaluating any, and returns a list of class "code", each element of
#' which is a \code{\link{promise}}. This extends,
#' e.g. \code{substitute(list(...))[[2]]} by capturing environments
#' along with expressions.
#'
#' \code{d <- dots(foo, quux=bar+baz)} captures the given unevaluated
#' arguments in a dots object, like \code{\link{alist}()}, but also
#' taking references to the present environment.)
#'
#' @param ... Any number of arguments.
#' @return A 'dots' object which captures the expressions and IDs of
#'   each argument.
#' @seealso \%()\%
#' @examples
#'
#' named.list <- function(...) {
#'  # Drop arguments that are not named
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


#' ...
#'
#' \code(dots_) is the standard-evaluating constructor for a dots
#' object.
#'
#' @rdname dots
#' @param promises A list of promise objects.
#' @seealso promise
#' @param exprs An expression or list of expressions (if
#'   \code{promises} is not given)
#' @param envs An environment or list of environments (if \code{promises}
#'   is not given)
dots_ <- function(promises, exprs, envs) {
  if (missing(promises)) {
    if (!is.list(exprs)) {
      exprs <- list(exprs)
    }
    if (!is.list(envs)) {
      envs <- list(envs)
    }
    structure(mapply(FUN=promise_, exprs, envs, SIMPLIFY=FALSE), class="dots")
  } else {
    as.dots(promises)
  }
}

#' Return an empty symbol.
#'
#' The missing value has two related uses. One is "at parse time" when
#' it is used to represent empty arguments. The other is "at run time"
#' when it is bound to function arguments that were not given any value.
#'
#' Manipulating expressions ("computing on the language") means we
#' have to deal with the first use case, because we have to be able to
#' make expressions that have empty arguments, like the first index in
#' \code{arr[,c]}.
#'
#' The second use of the missing sigil makes this tricky. Generally it
#' is a bad idea to assign a bare missing_value to a variable or use
#' one as the argument to a function, because this makes R think that
#' the variable \b{is} missing rather than \b{contains a} missing. For
#' instance, you can say
#'
#' \code{x <- list(missing_value(), 2, 3)}
#'
#' and get a valid list, but this:
#'
#' \code{a <- missing_value(); b <- 2; c <- 3
#' x <- list(a, b, c)}
#'
#' fails with an error about missing variable "a". When dealing with
#' missing_values, then, best to keep them wrapped up in lists.
#'
#' @param n Optional; a number. If provided, will return a list of
#' missing values with this many elements.
#' @return A symbol with empty name, or a list of such.
#' @seealso list_missing
#' @examples
#'
#' You may encounter values when programming using R expressions, 
#' # These statements are equivalent:
#' quote(function(x, y=1) x+y)
#' call("function", pairlist(x=missing_value(), y=1), call("+", as.name("x"), as.name("y"))
#'
#' # These statements are also equivalent:
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
#' @useDynLib promises _flist_to_dotsxp
#' @useDynLib promises _set_dots
#' @export
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
#' @useDynLib promises _dotsxp_to_flist
#' @useDynLib promises _get_dots
#' @export
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

#' Check if list items are equal to the "missing value."
#'
#' For \code{\dots} objects as made by \code{\link{dots}}, checks if
#' the expressions are missing.
#'
#' Checking for missing elements of ... without forcing by index is
#' important, for example in implementing array subsetting like
#' \code{`[`}. where a missing argument means to take all indexes on
#' that dimension.
#'
#' However there is not a good way to replicate
#' \code{`[`}'s behavior in base R; using \code{list(\dots)} to
#' collect all positional arguments will throw errors on missing
#' arguments. Instead, use \code{x <- list_missing(...)} and
#' \link{is.missing}(x) to detect missing arguments.
#'
#' @note A frequently seen strategy is to use
#'   \code{\link{match.call}(expand.dots=TRUE)} and
#'   \code{\link{eval}(..., parent.frame())} to screen for missing
#'   arguments while evaluating non-missing arguments. This is not
#'   recommended because \link{match.call} does not capture the
#'   environments of the arguments, leading to hygeine violations.
#'
#' @param x If given a list, compares each element with the missing
#'   value. Given a \code{\link{dots}} object, determines whether each
#'   element is missing. If the argument is a missing variable,
#'   returns scalar TRUE
#' @return For \code{is.missing}, a vector of boolean values.
#' @seealso missing_value
#' @export
is.missing <- function(x) if (missing(x)) TRUE else UseMethod("is.missing")

#' @export
is.missing.dots <- function(x) {
  vapply(exprs(x), identical, FALSE, missing_value())
}

#' @export
is.missing.default <- function(x) {
  if (identical(x, missing_value()))
    TRUE
  else if (is.list(x))
    vapply(x, identical, FALSE, quote(expr=))
  else
    rep(FALSE, length(x))
}

#' export
is.missing.promise <- function(x) {
  identical(expr(x), missing_value())
}

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

#' @export
value.dots <- function(d) {
  list %()% d
}

#' @export
value.promise <- function(d) {
  eval(body(d), environment(d))
}

#' @export
value.default <- function(f) value(as.promise(f))

#' @export
env <- function(x) UseMethod("env")

#' @export
expr <- function(x) UseMethod("expr")

#' @export
env.promise <- function(d) environment(d)

#' @export
expr.promise <- function(d) body(d)

#' @export
