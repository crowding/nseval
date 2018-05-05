`%||%` <- function(a, b) if (is.null(a)) b else a

#' Retreive lazy arguments from environments, by name.
#'
#' `arg` looks in the given environment for a binding,
#' without forcing any promises, and returns it as a [quotation].
#'
#' Generally, `arg(x)` is equivalent to `[unwrap](quo(x))` for
#' variable names x.
#'
#' @param sym The name to look up. For `arg` this is a symbol or
#'   character.
#' @param env The environment to look in. By default, the environment
#'   from which `sym` was passed.
#' @return `arg` returns a [quotation] object.
#' @note If you use a a literal character value, as in `arg_("x",
#'   environment())`, you MUST also give the environment parameter.The
#'   reason is that the R will discard scope information about code
#'   literals, depending on optinization settings; so when `arg_("x")
#'   is called in compiled code, the default value for `env` will be
#'   found to be [emptyenv()].
#' @export
arg <- function(sym,
                env = arg_env_(quote(sym), environment())) {
  sym_ <- arg_expr_(quote(sym), environment())
  arg_(sym_, env)
}


#' (arg)
#'
#' `args` looks up multiple variables, and returns a [dots] object.
#' `args(x, y)` is equivalent to `[unwrap](dots(x=x, y=y))`.
#'
#' If any of the given variables are not bound, an error will be raised.
#'
#' @param ... Bare names (not forced). Arguments may be named; these
#'   names determine the names on the output list. If argument names
#'   are not given, the input is used as output names
#' @return `args` returns a [dots] object.
#' @note Beware of writing `args(a, b, ...)` which probably doesn't do
#'   what you want. This is because R unwraps the symbol `...`
#'   occurring in argument lists before invoking `args`, so this ends
#'   up double-unwrapping `...`. For capturing `...` alongside named
#'   arguments you can use the syntax `args(x, y, (...))` (which is
#'   equivalent to `c(args(x, y), dots(...))`). You can also use
#'   `[get_call()]` to extract all function inputs.
#' @return `args` returns a `[dots]` object.
#' @seealso dots get_dots unwrap
#' @rdname arg
#' @export
#' @useDynLib nse _arg_dots
#' @useDynLib nse _dotsxp_to_flist
args <- function(...) {
  d <- dots(...)
  args_(exprs(d), envs(d))
}

#' arg
#'
#' `arg_` is the normally evaluating version of `arg_`;
#' `arg(x, e)` is equivalent to `arg_(quote(x), e)`.
#' @rdname arg
#' @export
#' @useDynLib nse _arg
arg_ <- function(sym, env = arg_env(sym, environment())) {
  .Call(`_arg`, env, as.name(sym), TRUE)
}

#' arg
#'
#' `args_` is a normally evaluating version of `args`;
#' `args(x, y)` is equivalent to `args_(alist(x, y))`.
#' @rdname arg
#' @param syms A character vector or list of names.
#' @param envs An environment, or a list of environments, to look for
#'   the bindings in.
#' @export
#' @useDynLib nse _arg_dots
#' @useDynLib nse _dotsxp_to_flist
args_ <- function(syms, envs) {
  if (!is.list(envs)) {
    envs <- rep(list(envs), length(syms))
  }
  dts <- .Call(`_arg_dots`, envs, syms, names(syms), TRUE)
  .Call(`_dotsxp_to_flist`, dts)
}


#' Determine which enclosing environment defines a name.
#'
#' @param sym A name. For `locate` the argument is used literally. For
#'   `locate_` it should have the value [name] or character.
#' @param env Which environment to begin searching from.
#' @param mode Either "any" or "function". "any" finds the lowest
#'   enclosing environment which defines a symbol. "function" finds an
#'   environment which defines the symbol as a function, possibly
#'   forcing promises along the way.
#' @return An environment object which defines `sym`, if one is found.
#' @note If you use a literal character argument, as in `locate("x",
#'   environment())`, you must also provide the environment
#'   argument. However `locate(x)` will work OK. See note under [arg].
#' @examples
#' # `locate` is useful if you want to implement something that works
#' # like [<<-], which updates the binding where it is found.
#' `<<-` <- function(lval, rval) {
#'  lval_ <- arg(lval_)
#'  rval_ <- arg(rval_)
#'  target.env <- locate_(expr(lval_), parent.env(env(lval_)))
#'  do_(quo(`<-`, target.env), lval_, rval_)
#' }
#'
#' x <- "not this one"
#' local({
#'   x <- "this one"
#'   local({
#'     x <- "not this one"
#'     x <<- "this works just like builtin <<-"
#'   })
#'   print(x)
#' })
#' @export
locate <- function(sym,
                   env = arg_env_(quote(sym), environment()),
                   mode = "any", ...) {
  sym_ <- arg_expr_(quote(sym), environment())
  locate_(sym = sym_, env = env, mode = mode, ...)
}

#' @rdname locate
#' @export
locate_ <- function(sym,
                    env = arg_env_(quote(sym), environment()),
                    mode = "any", ...) {
  # default arguments vs. s3 dispatch pitfall!
  # because UseMethod is going to force "sym",
  # which would make output of arg_env_ invalid,
  # I need to force "env" first...
  force(env)
  # and dispatch from a call with no default args
  locate.dispatch(sym=sym, env=env, mode=mode, ...)
}

locate.dispatch <- function(sym, env, mode, ...) {
  UseMethod("locate_")
}


#' locate
#'
#' If sym is a list (of [name]s) or a [dots] object, `locate` returns a list.
#' @rdname locate
#' @export
locate_.list <- function(sym,
                         env = arg_env_(quote(sym), environment()),
                         mode = "any", ...) {
  force(env)
  lapply(sym, locate_, env=env, mode=mode, ...)
}

#' locate
#'
#' When `sym` is a [quotation] or [dots], the `env` argument is ignored.
#' @rdname locate
#' @export
locate_.quotation <- function(sym,
                              env=(ignored),
                              mode = "any",
                              ...) {
  locate_(sym=expr(sym), env=env(sym), mode = mode, ...)
}

#' @rdname locate
#' @export
locate_.character <- function(sym,
                              env = arg_env_(quote(sym), environment()),
                              mode = "any",
                              ...) {
  if (length(sym) == 1) {
    locate_.name(sym=as.name(sym), env=env, mode=mode, ...)
  } else {
    # The "intuitive" thing here might be to return a list:
    # lapply(FUN=locate_.character, x, env=env, mode=mode, ...)
    # However this would not be type-stable (is "x" a vector of length
    # 1 or a singleton?)
    stop("use locate_.list for character vectors")
  }
}

# not exported.
`locate_.call` <- function(sym,
                           env = arg_env_(quote(sym), environment()),
                           mode = "any", ...) {
  locate_(sym[[2]], env=env, mode=mode)
}

#' @export
#' @rdname locate
`locate_.(` <- `locate_.call`


#' @export
locate_.dots <- function(sym,
                         env = (ignored),
                         mode = "any",
                         ...) {
  structure(lapply(sym, locate_, ...), class="dots")
}

#' @rdname locate
#' @param mode Either "any" or "function".
#' @param ifnotfound What is returned if the symbol is not found. By
#'   default an exception is raised.
#' @useDynLib nse _locate
#' @export
locate_.name <- function(sym,
                         env = arg_env_(quote(sym), environment()),
                         mode = "any",
                         ifnotfound = stop("Binding ", deparse(sym), " not found")) {
  # print(as.data.frame(get_call()))
  # browser()
  .Call(`_locate`,
        sym,
        env,
        switch(mode,
               "any" = FALSE,
               "function" = TRUE)
        ) %||% ifnotfound
}

#' Peel back layers of variable names from quotations.
#'
#' When an un[forced] [quotation]'s expression is a bare variable
#' name, `unwrap` follows the variable reference returns a
#' quotation. When the quotation is forced or has a nontrivial
#' expression `unwrap` has no effect.
#'
#' The syntax `locate( (...) )` is available for locating `...`.
#'
#' There are two good use cases for `unwrap(x, recursive=TRUE)`. One
#' is to derive plot labels (the most inoccuous use of
#' metaprogramming). Another is to check for missingness (this is what
#' R's [missing] and does as well).
#'
#' Using `unwrap(x, recursive=TRUE)` in other situations can get you
#' into confusing situations -- effectively you are changing the
#' behavior of a parent function that may be an unknown number of
#' levels up the stack, possibly turning a standard-evaluating
#' function into nonstandard-evaluating function. So recursive
#' unerapping is not the default behavior.
#' @export
#' @param x a [quotation] to unwrap.
#' @param recursive Default `FALSE` unwraps exactly once. If
#'   `TRUE`, unwrap as far as possible (until a forced promise or
#'   nontrivial expression is found.)
#' @return The [quotation] method returns a [quotation].
#' @examples
#' # different levels of unwrapping:
#' f <- function(x) { g(x) }
#' g <- function(y) { h(y) }
#' h <- function(z) {
#'   print(arg(z))
#'   print(unwrap(quo(z)))
#'   print(unwrap(unwrap(quo(z))))
#'   print(unwrap(quo(z), recursive=TRUE))
#' }
#'
#' w <- 5
#' f(w)

unwrap <- function(x, recursive=FALSE) {
  UseMethod("unwrap")
}

#' @export
unwrap.quotation <- function(x, recursive=FALSE) {
  .Call("_unwrap_quotation", x, recursive)
}


#' @export
#' @return The [dots] method returns a dots object with each component unwrapped.
unwrap.dots <- function(x, recursive=FALSE) {
  structure(lapply(x, function(x) .Call("_unwrap_quotation", x, recursive)), class="dots")
}

#' Turn quotations into bindings in the present environment.
#' @param dst A name,
#' @param src A [quotation] (or something that can be converted to a
#'   quotation, like a formula).
#' @seealso delayedAssign
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

