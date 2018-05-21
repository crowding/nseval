nse
======

<!-- [![CRAN version badge](http://www.r-pkg.org/badges/version/msgpack)](https://cran.r-project.org/package=msgpack) -->
[![Travis build status](http://travis-ci.org/crowding/nse.svg?branch=master)](https://travis-ci.org/crowding/nse)
[![Code coverage](https://codecov.io/gh/crowding/nse/branch/master/graph/badge.svg)](https://codecov.io/gh/crowding/nse)

`nse` is the missing API for non-standard evaluation and
metaprogramming in R. `nse` is intended to reflect R the way R
actually works.

## Installation

```
install.packages("devtools")
library(devtools)
install_github("crowding/nse")
```

### Why `nse` is needed

Before R, there was S, and S had some metaprogramming facilities,
using functions like `substitute`, `match.call`, `do.call`, `quote`,
`alist`, `eval`, and so on. R implemented that API. But S did not
have lexical scoping or the notion of an environment, whereas R
does. So R has been coping with a metaprogramming API that was not
designed with R's rules in mind. It turns out the S interface is not
sufficient to model R behavior, with consequences such as:

  * `match.call()` loses information about argument scopes, so normally
    occurring function calls often can't be captured in a reproducible
    form;
  * `do.call` can't reproduce many situations that occur in normal
    evaluation in R;
  * `parent.frame()` tells you something almost but not entirely
    unlike what you actually need to know in most situations;
  * it's difficult to wrap or extend nonstandard-evaluating functions;
  * it's difficult to use a nonstandard-evaluating function as an
    argument to a higher order function;
  * any mixture of metaprogramming and `...` rapidly turns painful;

and so on. As a result, R functions that use the S metaprogramming API
often end up with unintended behaviors that don't "fit" R: they lose
track of variable scope, suffer name collisions, are difficult to
compose, etc.

The good news is that you can simply replace most uses of
`match.call`, `parent.frame`, `do.call` and such with their
equivalents from `nse`, and may have fewer of these kinds of problems.

## Transitioning from base R to NSE

* Instead of `quote`, use `quo`.
* Instead of `match.call` or `sys.call`, use `get_call`, or `arg_list(x, y, (...) )`
* Instead of `substitute(x)`, use `arg(x)` (or `arg_expr(x)`)
* Instead of `substitute(list(...))[[2]]`, use `dots(...)`
* Instead of `do.call`, use `do`.
* Instead of `parent.frame`, use `arg_env` or `caller`.

### Who NSE is for

`nse` might be for you if:

* You've been befuddled by trying to use the above functions;
* You're been befuddled by other people's code that uses the above
  functions and need to work around it;
* You want to understand better what "goes on" "under the hood" when R
  is running.

### What `nse` does

`nse` introduces two S3 classes: `quotation`, and `dots`, which mirror
R's promises and `...`, respectively. Unlike their counterparts, these
are ordinary data objects, and can be manipulated as such.

* A `quotation` combines an R expression with an environment.  There
  are also `forced` quotations, which pair an expression with a value.
* A `dots` is a named list of quotations.

There is a set of consistently-named accessors and constructors for
capturing, constructing, and manipulating these objects.

`nse` also has a function `do` which is an enhanced `do.call`, and
`get_call` which is an improved `match.call`.

### What `nse` doesn't do

`nse` doesn't implement quasiquotation or hygeinic macros or code
coverage or DSLs or interactive debugging. But it is intended to be a
solid foundation to build those kinds of tools on!

`nse` doesn't introduce any fancy syntax -- the only nonstandard
evaluation in its own interface is name lookup and quoting, and
standard-evaluating equivalents are always also present.

`nse` doesn't try and remake all of R's base library, just the parts
about calls and lazy evaluation.

`nse` has no install dependencies and should play well with base R or any
other 'verse.

## Similar packages

Some other packages have been written with similar capabilities:

* [rlang](https://github.com/r-lib/rlang)
* [lazyeval](https://github.com/hadley/lazyeval)
* [pryr](https://github.com/hadley/pryr)
* [vadr](https://github.com/crowding/vadr), which this package is
  extracted and rewritten from.
