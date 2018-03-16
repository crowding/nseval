nse
======

`nse` is the missing API for non-standard evaluation and
metaprogramming in R. `nse` is intended to reflect R the way R
actually works.

### Why `nse` is needed

In the beginning, there was S, and S had metaprogramming built in,
using functions like `substitute`, `match.call`, `do.call`, `quote`,
`alist`, `eval`, and so on. R was made to emulate those facilities.
But S did not have lexical scoping or the notion of an environment,
whereas R does. It turns out the S interface is not sufficient to
capture R behavior, with consequences such as:

  * `match.call()` loses information about argument scopes so normally
    occurring function calls often can't be captured in a reproducible
    form;
  * `do.call` can't reproduce many situations that occur in normal
    evaluation in R;
  * `parent.frame()` tells you something almost but not entirely
    unlike what you actually need to know in most situations;
  * it's difficult to wrap around nonstandard-evaluating functions;
  * it's difficult to use a nonstandard-evaluating function as an
    argument to a higher order function;
  * any mixture of metaprogramming and `...` rapidly turns painful;

and so on. As a result, R functions that use the S metaprogramming API
often end up with unintended behaviors that don't "fit" R: they lose
track of variable scope, suffer name collisions, can't be easily
wrapped, and so on.

The good news is that you can simply replace most uses of
`match.call`, `parent.frame`, `do.call` and such with their
equivalents from `nse`, and may have fewer of these kinds of problems.

### Who NSE is for

`nse` might be for you if:

* You've been befuddled by trying to use the above functions;
* You're been befuddled by other people's code that uses the above
  functions and need to work around it;
* You want to understand better what "goes on" "under the hood" when R
  is running.

### What `nse` does

`nse` introduces three S3 classes: `quotation`, `dots`, and `call`,
which mirror R's promises, `...`, and function invocations,
respectively. Unlike their counterparts, these are ordinary data
objects, and can be manipulated as such.

* A `quotation` combines an R expression with an environment.  There
  are also `forced` quotations, which pair an expression with a value.

* A `dots` is a named list of quotations.

* A `call` is a quotation (the head of the call) combined with a dots
  (the arguments).

There is a set of consistently-named accessors and constructors for
capturing, constructing, and manipulating these objects.

### What `nse` doesn't do

`nse` does not implement quasiquotation or hygeinic macros or DSLs or
pattern matching or static analysis or automated code refactoring, but
it is intended to be a solid foundation to build those kinds of
facilities on!

`nse` doesn't introduce new syntax -- the only nonstandard evaluation
in its own interface is name lookup and quoting, and
standard-evaluating equivalents are always there.

# EXAMPLE

Using `arg_env` instead of `parent_frame` also allows non-standard-evaluating
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
that while in Python variadic arguments are stored in first-class data
structures, in R the `...` is opaque. It's tricky to do things like
take the second argument, or take all the odd arguments, or obtain a
list of argument names without forcing them, or concatenating two
argument lists together. The only thing you can do with a `...` when
you have it is to pass them all to another function's argument list.

But under the hood, a `...` is just a named list of promises. The
`nse` package provides a `dots` class and accessor functions that
let you manipulate sequences of promises the same way as you would
manipulate other kinds of lists.

As an example, consider trying to implement the R library function
`switch`. This takes any number of arguments, and uses the argument
`expr` -- either a number or a name -- to decide which of the other
arguments to return, only forcing that argument. But in base R it is
difficult to access unevaluated arguments by index or name, so
`switch` has a C implementation.

It's actually possible to implement `switch` in base R, but... coming
up with a solution requires a bit of tinkering. I can do it by
synthesizing a function with the right arglist to match the required
argument. The weirdest part was working out how names like `..3` work.

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
  force_(dots(...)[[expr]])
}
```

## Missing arguments.

When a function defined as having an argument but that argument is not
given, that argument is "missing."

For example,

f <- function(x) missing(x)
f(1)
f()

## Similar paackages

Some other packages have been written along similar lines:

* rlang
* lazyeval
* pryr
* vadr, which this package is a rewrite from.
