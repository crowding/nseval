`%||%` <- function(a, b) if (is.null(a)) b else a

#' Capture named arguments from the present environment, returning a dots list.
#'
#' @param ... Variable names (unevaluated). Arguments may be named; these names
#' determine the names on the output list.
#' @return a \code{\link{dots}} object containing the promises that are bound to
#' those variables in the calling environment.
#'
#' Note that `args(a, b, ...)` probably doesn't do what you want. This
#' is because R unwraps `...` before invoking `args`, so this ends up
#' double-unwrapping `...`. You can avoid this using the syntax
#' `args(x, y, (...))` (which is equivalent to `c(args(x, y), dots(...))`)
#' @seealso dots get_dots
#' @rdname arg_list
#' @export
#' @useDynLib nse _arg_dots
#' @useDynLib nse _dotsxp_to_flist
args <- function(...) {
  d <- dots(...)
  args_(exprs(d), envs(d))
}

#' Capture an argument by name from the present environment.
#'
#' @param sym the name to look up. For \code{arg} this is a symbol
#'   and not evaluated; for \code{arg_} this is a symbol or
#'   character.
#' @rdname arg
#' @return A [quo] object.
#' @export
arg <- function(sym,
                env = arg_env_(quote(sym), environment())) {
  sym_ <- arg_expr_(quote(sym), environment())
  arg_(sym_, env)
}

#' @param env  The environment to look in.
#' @rdname arg
#' @export
#' @useDynLib nse _arg
arg_ <- function(name, env = arg_env(name, environment())) {
  force(env)
  .Call(`_arg`, env, as.name(name), TRUE)
}

#' `args_` is a normally evaluating version of `args`.
#'
#' @rdname arg_list
#' @param names A character vector or list of names.
#' @param envs An environment, or list of environments, to look for
#'   the bindings in.
#' @export
#' @useDynLib nse _arg_dots
#' @useDynLib nse _dotsxp_to_flist
args_ <- function(syms, envs) {
  if (!is.list(envs)) (envs = rep(list(envs), length(syms)))
  dts <- .Call(`_arg_dots`, envs, syms, names(syms), TRUE)
  .Call(`_dotsxp_to_flist`, dts)
}

#' Get environment or expression from a named argument.
#'
#' \code{arg_env} finds the origin of an expression of an argument
#' bound in the present environment. It is equivalent to `env(arg(x))`
#' is further equivalent to `env(unwrap(quo(x)))`.
#'
#' @rdname arg_env
#' @param name A single argument name; not evaluated.
#' @param env The environment to look in.
#' @export
#' @useDynLib nse _arg_env
arg_env <- function(name,
                    env = arg_env_(quote(name), environment())) {
  name_ <- arg_expr_(quote(name), environment())
  arg_env_(name_, env)
}

#' _arg_env(x, env) is the normally evaluating version of arg_env; you
#' supply a vatiable name and an environment to look in.
#'
#' @export
#' @useDynLib nse _arg_env
arg_env_ <- function(name,
                     env = arg_env_(quote(name), environment())) {
  .Call(`_arg_env`, env, as.name(name), TRUE)
}

#' \code{arg_expr(x)} is a shortcut for `expr(arg(x))`. It fetches the
#' expression attached to a promise in the present environment.
#'
#' @rdname arg_env
#' @export
#' @useDynLib nse _arg_expr
arg_expr <- function(name,
                     env=arg_env_(quote(name), environment())) {
  name_ <- arg_expr_(quote(name), environment())
  arg_expr_(name_, env)
}

#' \code{arg_expr_} is the normally evaluating version of arg_expr.
#' @rdname arg_env
#' @export
#' @useDynLib nse _arg_expr
arg_expr_ <- function(name,
                      env=arg_env_(quote(name), environment())) {
  .Call(`_arg_expr`, env, as.name(name), TRUE)
}

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

#' \code{is_promise_} is a normally evaluating version of \code{is_promise}.
#' @rdname arg_env
#' @export
#' @useDynLib nse _is_promise
is_promise_ <- function(syms, envs)
{
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
     syms,
     if (is.list(envs)) envs else list(envs),
     FUN=function(sym, env) {
       .Call(`_is_promise`, env, as.name(sym), TRUE)
     })
}

#' Detect if named arguments are missing.
#'
#' `is_missing(...)` is similar to [missing] but can take multiple
#' arguments.
#'
#' `is_missing_` is a normally evaluating equivalent of
#' `is_missing`
#'
#' @rdname arg_env
#' @param syms A character vector or list of symbols.
#' @param envs A list of environment objects.
#' @param recursive Whether to recursively descend through unforced
#'   promises. `TRUE` mimics the behavior of [missing].
#' @export
#' @useDynLib nse _is_missing
is_missing_ <- function(syms, envs, recursive=TRUE) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  if (!is.list(envs)) envs <- list(envs)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call(`_is_missing`, env, as.name(sym), unwrap)
    })
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
is_forced_ <- function(syms, envs) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
     syms,
     if(is.list(envs)) envs else list(envs),
     FUN=function(sym, env) {
       .Call(`_is_forced`, env, as.name(sym), TRUE)
     })
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
is_literal_ <- function(syms, envs, warn=TRUE) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call(`_is_literal`, env, as.name(sym), TRUE)
    })
}

is_missing <- function(...) {
  d <- dots(...)
  is_missing_(exprs(d), envs(d))
}

is_missing_ <- function(syms, envs) {
  if (is.null(names(syms)))
    names(syms) <- as.character(syms)
  mapply(
    syms,
    if (is.list(envs)) envs else list(envs),
    FUN=function(sym, env) {
      .Call(`_is_missing`, env, as.symbol(sym), TRUE, TRUE)
    })
}

#' Determine which enclosing environment defines a name.
#'
#' `locate` is useful if you want to implement something that works
#' like `<<-`, which updates the binding where it is found.
#'
#' @param x A name.
#' @param mode Either "any" or "function".
#' @examples
#' `<<-` <- function(lval, rval) {
#'  lval <- arg(lval)
#'  rval <- arg(rval)
#'  target.env <- find_(expr(lval), parent.env(env(lval)))
#'  do_(quo(`<-`, target.env), lval, arg(rval))
#' }
#' @export
locate <- function(sym,
                   env = arg_env_(quote(sym), environment()),
                   mode = "any", ...) {
  sym_ <- arg_expr_(quote(sym), environment())
  locate_(sym = sym_, env = env, mode = mode, ...)
}

#' `locate_` is the normally evaluating version of
#' locate; it takes a [name] or a character and an environment.
#' @rdname locate
#' @return an environment.
#' @export
locate_ <- function(sym,
                    env = arg_env(quote(sym), environment()),
                    mode = "any", ...) {
  UseMethod("locate_")
}

#' The `locate_` method for quotations uses the expr and environment together.
#' @rdname locate
#' @export
locate_.quotation <- function(sym, ..., mode = "any") {
  locate_(sym=expr(sym), env=env(sym), mode = mode, ...)
}

#' @rdname locate
#' @param env Which environment to begin searching from.
#' @export
locate_.character <- function(sym, env=arg_env(x, environment()), mode="any", ...) {
  if (length(x) == 1) {
    locate_.name(sym=as.name(sym), env=env, mode=mode, ...)
  } else {
    lapply(FUN=locate_.character, x, env=env, mode=mode, ...)
  }
}

#' The list method accepts a list of [names](name), and returns a list of
#' [environments](environment).
#' @rdname locate
#' @export
locate_.list <- function(sym, ...) {
  lapply(FUN=locate_, sym, ...)
}

#' @rdname locate
#' @param mode Either "any" or "function".
#' @param ifnotfound What is returned if the name is not found.
#' @useDynLib nse _locate _locate_all
#' @export
locate_.name <- function(sym,
                         env = arg_env_(quote(sym), environment()),
                         mode = "any",
                         ifnotfound = stop("Binding ", deparse(sym), " not found")) {
  .Call(`_locate`,
        sym,
        env,
        switch(mode,
               "any" = FALSE,
               "function" = TRUE)
        ) %||% ifnotfound
}

#' If an unforced quotation's expression is a variable name, retrieve the
#' binding that is referenced.
#'
#' In general, `nse` direct accessors like `arg_expr(x)` can be
#' translated to `expr(unwrap(quo(x)))`.
#'
#' There are two good use cases for unwrap(x, recursive=TRUE). One is
#' to derive axis labels (a.k.a the most inoccuous use of
#' metaprogramming). Another is to check for missingness (this is what
#' R's [missing] does as well).
#'
#' Using unwrap(x, recursive=TRUE) in other situations can get you
#' into confusing situations -- effectively you are changing the
#' behavior of a parent function that may be several levels up the
#' stack, possibly turning a standard-evaluating function into
#' nonstandard-evaluating function.
#' @export
#' @param recursive If `FALSE`, the default, unwrap exactly once and
#'   throw an error If `TRUE`, and the referred variable is If the
#'   binding is another unforced variable name, recursively unwrap
unwrap <- function(x, recursive=FALSE) {
  UseMethod("unwrap")
}

#' @export
unwrap.quotation <- function(x, recursive=FALSE) {
  .Call("_unwrap_quotation", x, recursive)
}


#' @export
unwrap.dots <- function(x, recursive=FALSE) {
  structure(lapply(x, function(x) .Call("_unwrap_quotation", x, recursive)), class="dots")
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

