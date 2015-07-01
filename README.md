fexpr
======

The `fexpr` package provides an interface to R's promise objects and
dot (...) argument lists.

A "[fexpr](wiki)" is a type of nonstandard-evaluating function present in
certain languages of the Lisp family. Fexprs [can be used][shutt] to
to implement syntactic abstractions such as new types of control flow,
or domain-specific languages. R's implementation of lazy evaluation in
terms of promises essentially means that ordinary R functions are
fexprs.

[shutt]: http://www.wpi.edu/Pubs/ETD/Available/etd-090110-124904/
[wiki]: https://en.wikipedia.org/wiki/Fexpr

R's traditional tools for doing nonstandard evaluation (`substitute`,
`parent.frame`, `match.call`) are non-orthogonal and tend to exhibit
some scope problems in use. Using `fexpr` interface, nonstandard
evaluating functions can be constructed using a more orthogonal,
explicit style.

## Access to promises

R functions evaluate their arguments lazily. When R invokes a
function, the function arguments are lazily bound to _promises_.  Each
promise is a triple of three values: the environment in which the
argument appeared, the expression that

The `fexpr` package allow inspection of each part of a promise.

### Why to use `arg_env`, not `parent.frame`

Here's a problem that happens with traditional R metaprogramming....

## Argument lists and `...`

Promises can be put into a special type of list.

Variadic arguments (`...`) and missing values are two of the trickiest
spots of R's semantics, and there are very few tools to work with them
-- besides `missing` there's `substitute` and `do.call`, both of which
are hairy and mostly serve other purposes. Mostly people treat `...`
as an opaque block to pass along to another function. This package
contains a number of functions that let you work explicitly with `...`
lists, concatenating and subsetting them, while still allowing R's
lazy-evaluation semantics to do the right thing. So a function using
`dots` can decide whether and when to evaluate each of its unnamed
arguments:

```r
inSomeOrder <- function(...) invisible(list %()% sample(dots(...)))
inSomeOrder(print("Boing!"), print("Boom"), print("Tschack!"), print("Ping"),
            print("Zong"), print("Pssh"))
# [1] "Boing!"
# [1] "Zong"
# [1] "Ping"
# [1] "Boom"
# [1] "Pssh"
# [1] "Tschack!"
```

For a more pointed example, consider `switch`. Switch takes its first
argument and uses it to decide which if its subsequent arguments to
evaluate.

Consider trying to implement an R function that has the behavior of
`switch` properly (not as a C function, and not inspecting the stack
using `match.call()` or `parent.frame()` which are evil.) This is
doable in pure R but wacky and slow -- the only way I can see to
selectively evaluate one named argument is to build a function that
takes that argument:

```r
switch2 <- function(expr, ...) {
  n <- names(substitute(list(...)))[-1]
  if (!is.null(n))
      arglist <- as.pairlist(structure(
          rep(list(quote(expr=)), length(n)),
          names=n))
  else
      (arglist <- as.pairlist(alist(...=)))

  if (is.numeric(expr))
      body <- as.name(paste0("..", expr))
  else
      body <- as.name(expr)
  f <- eval(substitute(`function`(arglist, body),
                         list(arglist=arglist, body=body)))
  f(...)
}
```

But with a direct interface to manipulate dotlists, `switch` is easy:

```r
switch3 <- function(expr, ...) {
  dots(...)[[expr]]
}
```

You may also use `dots_unpack()` to inspect the contents of
as-yet-unevaluated dots objects, exposing R's promise mechanism:

```r
x <- 1
y <- 2
d <- dots(a=x, b=y, c=x+y)
unpack(d)
#   name         envir  expr value
# a    a <environment>     x  NULL
# b    b <environment>     y  NULL
# c    c <environment> x + y  NULL
# > y <- 3
(function(b, ...) b) %()% d #force the "b" slot to evaluate
# [1] 3
unpack(d)
#   name         envir  expr value
# a    a <environment>     x  NULL
# b    b          NULL     y     3
# c    c <environment> x + y  NULL
c %()% d
# a b c
# 1 3 4
> unpack(d)
#   name envir  expr value
# a    a  NULL     x     1
# b    b  NULL     y     3
# c    c  NULL x + y     4
```

## Missing arguments.

In R, function arguments may be "missing." When this happens...
