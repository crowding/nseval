promises
======

The `promises` package implements a tidy, (hygeinic) API to
nonstandard evaluation in R, covering nonstandard evaluation,
laziness, promises, and "..."  argument lists.

[shutt]: http://www.wpi.edu/Pubs/ETD/Available/etd-090110-124904/
[wiki]: https://en.wikipedia.org/wiki/Fexpr

R's traditional tools for doing nonstandard evaluation (`substitute`,
`parent.frame`, `match.call`) are non-orthogonal and tend to exhibit
some scope problems in use. Using the `promises` package, nonstandard
evaluation can be written in a more explicit style.

## Laziness via promises

As you probably know, R evaluates function arguments lazily. In R's
case laziness is implemented via _promises_. A promise is a data
object that exists in either a forced or unforced state. In the forced
state it contains a value, while in the unforced state it contains a
recipe to create a value (in R, an expression and an environment to
evaluate it in.)

When R invokes a function, the function arguments are bound to
promises. During standard evaluation, when the R interpreter requires
the value of a variable which is bound to an unforced promise, R
evaluates the expression contained in the promise. Then the promise is
converted to a forced promise.

_Nonstandard evaluation_ is the bypassing of the standard evaluation
process using functions that inspect promises without forcing
them. The `promises` package contains a set of useful functions for
inspecting promises, including `arg_env` which accesses the
environment of unforced promises (which is not otherwise exposed in
R's standard library.)

### Why to use `arg_env`, not `parent.frame`

The problem with parent.frame() is that it prevents a function from
being wrapped.

Using `arg_env` instead of `parent.frame()` makes it easier to wrap
functions that do nonstandard evaluation. Here's an example.

# EXAMPLE

Using arg_env instead of parent_frame also allows non-standard-evaluating
functions to be composable. (example with dlply and glm functions)

## caller and with_caller

In writing NSE functions you should as much as possible use `arg_env`
to determine where arguments come from. First convert all uses of
`parent.frame()` into `arg_env()`.  Where a function is called from is
another thing, called `caller()`. This is actually a bit different
from what `parent.frame()` does. `caller()` guards against giving
surprising (wrong) results in situations involving lazy evaluation or
when the caller can no longer be determined -- it prefers to throw
errors instead.

## Argument lists and `...`

In R, to write a function that takes any number of arguments, we use
the symbol `...`, which represents a sort of list of arguments. This
is somewhat similar to Python's solution of `*args, **kwargs`, except
that while in Python variadic arguments are stored in first-calss data
structures, in R the `...` is opaque. It's tricky to do things like
take the second argument, or take all the odd arguments, or obtain a
list of argument names, or concatenate two argument lists
together. The only thing you can do with a `...` when you have it is
to pass them all to another function's argument list.

But under the hood, a `...` is just a named list of promises. The
`promises` package provides a `dots` class and accessor functions that
let you manipulate sequences of promises the same way as you would
manipulate other kinds of lists.

As an example, consider trying to implement the R library function
`switch`. This takes any number of arguments, and uses the argument
`expr` -- either a number or a name -- to decide which of the other
arguments to evaluate. But "accessing unevaluated arguments by index
or name" isn't easy expressible in pure R, so `switch` has a C
implementation.

It's actually possible to implement `switch` in pure R, but... coming
up with a solution requires more under-the-hood R mechanics than you
might care to know. I can do it by synthesizing a function with the
right arglist to match the required argument:

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

The point of this digression is that with a direct interface to
manipulate argument lists, `switch` is easy:

```r
switch3 <- function(expr, ...) {
  dots(...)[[expr]]
}
```

## Missing arguments.

When a function  defined as having an argument but that argument is not given, that argument is "missing.

For example,

f <- function(x) missing(x)
f(1)
f()


