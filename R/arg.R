`%||%` <- function(a, b) if (is.null(a)) b else a

#' Capture lazy variables as quotations.
#'
#' `arg(x)` looks in the calling environment for the binding `x`,
#' taken literally, and returns it as a [quotation]. `arg(x)` is
#' equivalent to `unwrap(quo(x))`.
#'
#' @param sym The name to look up. For `arg` this is a literal name,
#'   not evaluated. For `arg_` this should evaluate to a symbol or
#'   character.
#' @param env The environment to look in. By default, the environment
#'   from which `sym` was passed.
#' @return `arg` returns a [quotation] object.
#' @note If you use a a literal character value, as in `arg_("x",
#'   environment())`, you MUST also give the environment
#'   parameter. The reason is that R will discard scope information
#'   about code literals in byte-compiled code; so when `arg_("x")` is
#'   called in compiled code, the default value for `env` will be
#'   found to be [emptyenv()].
#' @export
arg <- function(sym,
                env = arg_env_(quote(sym), environment())) {
  sym_ <- arg_expr_(quote(sym), environment())
  arg_(sym_, env)
}

#' @rdname arg
#' @description
#' `arg_` evaluates the first element normally;
#' `arg(x, e)` is equivalent to `arg_(quote(x), e)`.
#' @export
#' @useDynLib nseval _arg
arg_ <- function(sym, env = arg_env(sym, environment())) {
  .Call("_arg", env, as.name(sym), TRUE)
}

#' @rdname arg
#' @description
#' `arg_list` looks up multiple variables, and returns a [dots]
#' object.  `arg_list(x, y)` is equivalent to `unwrap(dots(x=x,
#' y=y))`. If any of the requested variables are not bound, an error
#' will be raised.
#'
#' @param ... Bare names (not forced). Arguments may be named; these
#'   names determine the names on the output list. If arguments are
#'   not named, the names given are used.
#' @return `args` returns a [dots] object.
#' @note Beware of writing `arg_list(a, b, ...)` which probably
#'   doesn't do what you want. This is because R unwraps `...` before
#'   invoking `arg_list`, so this ends up double-unwrapping `...`. To
#'   capture `...` alongside named arguments you can use the syntax
#'   `arg_list(x, y, (...))` (which is equivalent to `c(arg_list(x,
#'   y), dots(...))`). You can also use [`get_call()`] to extract all
#'   function inputs to an active function.
#' @return `arg_list` returns a [dots] object.
#' @seealso dots get_dots unwrap
#' @export
#' @useDynLib nseval _arg_dots
#' @useDynLib nseval _dotsxp_to_flist
arg_list <- function(...) {
  d <- dots(...)
  arg_list_(exprs(d), envs(d))
}


#' @rdname arg
#' @description
#' `arg_list_` is a normally evaluating version of `arg_list`;
#' `arg_list(x, y)` is equivalent to
#' `arg_list_(alist(x, y), environment())`.
#' @param syms A character vector or list of names.
#' @param envs An environment, or a list of environments, to look for
#'   the bindings in.
#' @export
#' @useDynLib nseval _arg_dots
#' @useDynLib nseval _dotsxp_to_flist
arg_list_ <- function(syms, envs) {
  if (!is.list(envs)) {
    envs <- rep(list(envs), length(syms))
  }
  dts <- .Call("_arg_dots", envs, syms, names(syms), TRUE)
  .Call("_dotsxp_to_flist", dts)
}


#' @rdname arg
#' @description
#' `set_arg` and set_arg_ create bindings from quotations. They
#' replace base function [delayedAssign].
#' @param dst A name; for `set_arg` this is quoted literally; for
#'   `set_arg_` this should be a [quotation].
#' @param src A [quotation] (or something that can be converted to a
#'   quotation, like a formula).
`set_arg` <- function(dst, src)  {
  dst_ <- arg_(quote(dst), environment())
  set_arg_(dst_, src)
}

#' @export
#' @rdname arg
#' @useDynLib nseval _quotation_to_promsxp
`set_arg_` <- function(dst, src) {
  dst <- as.quo(dst)
  dstname <- expr(dst)
  if (dstname == "..." || identical(dstname, quote( (...) ))) {
    stop("Use set_dots to set `...`")
  } else {
    src <- as.quo(src)
  }
  switch(mode(dstname),
         name = assign(as.character(dstname),
                       envir=env(dst),
                       .Call("_quotation_to_promsxp", src)),
         character = assign(dstname,
                            envir=env(dst),
                            .Call("_quotation_to_promsxp", src)),
         call = stop("Subassignment with set_arg not supported"),
         stop("Don't know how to assign to a ", typeof(expr(dst))))}
