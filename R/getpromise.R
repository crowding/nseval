`%||%` <- function(a, b) if (is.null(a)) b else a

#' Fetch argument from an environment by name, returning a dots list.
#'
#' @param ... Variable names (unevaluated). Arguments may be named; these names
#' determine the names on the dots list (and not the variable names).
#' @return a \code{\link{dots}} object containing the promises that are bound to
#' those variables in the calling environment.
#' @note The tags on the dots object are determined by argument names;
#' variable names are discarded.
#' @seealso dots get_dots
#' @rdname arg_list
#' @export
#' @useDynLib nse _arg_dots
#' @useDynLib nse _dotsxp_to_flist
args <- function(...) {
  d <- dots(...)
  dts <- .Call(`_arg_dots`, envs(d), exprs(d), names(d))
  .Call(`_dotsxp_to_flist`, dts)
}


#' Fetch an argument by name from the present environment.
#'
#' @param name the name to look up. For \code{arg} this is a symbol
#'   and not evaluated; for \code{arg_} this is a symbol or
#'   character.
#' @rdname arg
#' @return A [quo] object.
#' @export
arg <- function(name) {
  pr <- arg_(quote(name), environment())
  arg_(body(pr), environment(pr))
}

#' @param env  The environment to look in.
#' @rdname arg
#' @export
#' @useDynLib nse _arg
arg_ <- function(name, env) {
  .Call(`_arg`, env, as.name(name), TRUE)
}

#' `args_` is a normally evaluating version of `args`.
#'
#' @rdname arg_list
#' @param names A character vector or list of names.
#' @param envs An environment, or list of environments, to look for
#'   the bindings in.
#' @param tags An optional character vector specifying output variable
#'   names. If this is NULL, the input names themselves will be
#'   duplicated. By default this uses the names attribute of the names
#'   argument.
#' @export
#' @useDynLib nse _arg_dots
#' @useDynLib nse _dotsxp_to_flist
args_ <- function(names,
                  envs,
                  tags = names(names)) {
  names <- lapply(names, as.name)
  if (!is.list(envirs)) (envirs = rep(list(envirs), length(names)))
  dts <- .Call(`_arg_dots`, envirs, names, tags, TRUE)
  .Call(`_dotsxp_to_flist`, dts)
}

#' Get environment or expression from a named argument.
#'
#' \code{arg_env} determines the lexical scope of an argument bound in
# the present environment.
#' @rdname arg_env
#' @param name A single argument name; not evaluated.
#' @param env The environment to look for the argument name in.
#' @export
#' @useDynLib nse _arg_env
arg_env <- function(name,
                    env=arg_env(name, environment())) {
  .Call(`_arg_env`, env, substitute(name), TRUE)
}

#' @export
#' @useDynLib nse _arg_env
arg_env_ <- function(name,
                     envir=arg_env(name, environment())){
  .Call(`_arg_env`, envir, as.name(name), TRUE)
}


#' ...
#'
#' \code{arg_expr(x)} is a shorthand for expr(arg(x)). It fetches the
#' expression attached to a promise in some environment.
#' environment. The effect is similar to \code{substitute(name)}.
#'
#' @rdname arg_env
#' @export
#' @useDynLib nse _arg_expr
arg_expr <- function(name,
                     envir=arg_env(name, environment())) {
  .Call(`_arg_expr`, envir, substitute(name), TRUE)
}

#' ...
#'
#' \code{arg_expr_} is the normally evaluating version of arg_expr.
#' @rdname arg_env
#' @export
#' @useDynLib nse _arg_expr
arg_expr_ <- function(name, envir=arg_env(name, environment()))
{
 .Call(`_arg_expr`, envir, as.name(name), TRUE)
 }

#' ...
#'
#' \code{is_promise} returns TRUE if a named variable is bound to a
#' promise. It returns a boolean vector with one entry for each name
#' given. An error is raised if a binding does not exist.
#'
#' @rdname arg_env
#' @export
#' @param ... Any number of variable names; not evaluated.
is_promise <- function(...) {
  d <- dots(...)
  is_promise_(exprs(d), envs(d))
}

#' ...
#'
#' \code{is_promise_} is a normally evaluating version of \code{is_promise}.
#' @rdname arg_env
#' @param names names of arguments to look up.
#' @export
#' @useDynLib nse _is_promise
is_promise_ <- function(names, envirs)
{
  mapply(
    FUN=function(name, envir) {
      .Call(`_is_promise`, envir, name, TRUE)
    },
    lapply(names, as.name),
    if (is.list(envirs)) envirs else list(envirs))
 }

#' Detect if named arguments are missing.
#'
#' `is_missing(...)` is similar to "missing" but returns a named
#' logical vector.
#'
#' \code{is_missing_} is a normally evaluating equivalent of
#' \code{\link{is_missing}}. It takes a number of names and environments,
#' and checks whether the names are bound to missing arguments.
#'
#' @rdname arg_env
#' @export
#' @useDynLib nse _is_missing
is_missing_ <- function(names, envirs) {
  names <- lapply(names, as.name)
  if (!is.list(envirs)) envirs <- list(envirs)
  mapply(
    function(name, envir) {
      .Call(`_is_missing`, envir, name)
    },
    names,
    envirs)
}

#' ...
#'
#' \code{is_forced} returns FALSE if an argument is bound to a promise that
#' has not yet been forced, TRUE otherwise. An error is raised if a binding
#' does not exist.
#'
#' @rdname arg_env
#' @export
is_forced <- function(...) {
  d <- dots(...)
  is_forced_(exprs(d), envs(d))
}

#' ...
#'
#' \code{is_forced_} is a normally evaluating version of \code{is_forced}.
#' @rdname arg_env
#' @export
#' @useDynLib nse _is_forced
is_forced_ <- function(names, envirs) {
  mapply(
    FUN=function(name, envir) {
      .Call(`_is_forced`, envir, name, TRUE)
    },
    lapply(names, as.name),
    if(is.list(envirs)) envirs else list(envirs))
}

#' ...
#'
#' \code{is_literal} returns TRUE if a binding is (or could be) a
#' source literal. This includes singleton vectors and missing
#' values. (Depending on optimization settings, R will often not
#' bother constructing promises to wrap a literal.)
#' @rdname arg_env
#' @export
is_literal <- function(...) {
  d <- dots(...)
  is_literal_(exprs(d), envs(d))
}

#' ...
#'
#' \code{is_literal_} is a normally evaluating version of \code{is_literal}.
#' @rdname arg_env
#' @export
#' @useDynLib nse _is_literal
is_literal_ <- function(names, envs, warn=TRUE) {
  mapply(FUN=function(name, envir) {
    .Call(`_is_literal`, envir, as.name(name), TRUE)
  },
  names,
  if (is.list(envirs)) envirs else list(envirs))
}

is_missing <- function(...) {
  d <- dots(...)
  is_missing_(exprs(d), envs(d))
}

is_missing_ <- function(names, envs) {
  mapply(
    FUN=function(name, env) {
      .Call(`_is_missing`, env, as.name(name), TRUE)
    },
    names,
    if (is.list(envs)) envs else list(envs))
}

#' Find the enclosing environment that defines a name.
#' @param x A name.
#' @param mode Either "any" or "function".
#' @rdname find
#' @export
find <- function(x, mode="any") {
  find_(arg_expr(x), arg_env(x, environment()))
}


#' `find_` is the normally evaluating version, which takes an environment.
#' @rdname find
#' @export
#' @param 
find_ <- function(x, env, mode="any",
                  ifnotfound = stop("Binding ", x, " not found")) {
  result <- .Call(`_find`,
                  as.name(x),
                  env,
                  switch(mode,
                         any = FALSE,
                         "function" = TRUE))
  result %||% ifnotfound
}

#' If a quotation is an unevaluated symbol, find the promise through
#' its source envronments, potentially iterating through several
#' unforced promises without forcing any. If at any point we come to a
#' promise with a more-complicated expression, we use that.
#'
#' If something in the promise chain refers to a nonexistent variable,
#' return an empty promise (i.e. `quo()`, which can be tested for using `missing_`).
#'
#' This process is used by the builtin [missing], so this algorithm,
#' is used by `missing_`. It can also be useful to derive axis labels.
#' @export
unwrap <- function(x) {
  UseMethod("unwrap", x)
}

#' @export
unwrap.quotation <- function(x) {
  .Call("_unwrap_quotation", x)
}

#' @export
unwrap.dots <- function(x) {
  structure(lapply(x, function(x) .Call("_unwrap_quotation", x)), class="dots")
}

#' Bind a name in an environment to a promise.
#' @param q a quotation.
#' @param env The environment to store in.
#' @param name The name to use. If "" or NULL, will append to "...".
quo2env <- function(q, env, name) {
  q <- as.quo(q)
  d <- as.dots(q)
  names(d) <- as.character(name)
  dots2env(d, env)
}

#' Turn quotations into bindings in the present environment.
#' @param dst A name,
#' @param src A [quotation] (or something that can be converted to a
#'   quotation, like a formula).
`%<-%` <- function(dst, src)  {
  dst <- arg(dst)
  UseMethod("%<-%", src)
}

`%<-%.default` <- function(dst, src) {
  src <- as.quo(src)
  .Class <- class(src)
  NextMethod("%<-%", src)
}

#' @useDynLib nse _quotation_to_promsxp
`%<-%.quotation` <- function(dst, src) {
  dst <- arg(dst) # Interesting... dst is not remembered from the
                  # dispatch function. The original call is
                  # replicated?
  switch(mode(expr(dst)),
         name = assign(as.character(expr(dst)),
                       envir=env(dst),
                       .Call(`_quotation_to_promsxp`, src)),
         call = stop("Subassignment with %<-% not implemented"),
         stop("Don't know how to put a quotation into a ", typeof(expr(dst))))
}
