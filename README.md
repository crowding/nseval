NSEval
======

[![CRAN version badge](http://www.r-pkg.org/badges/version/nseval)](https://cran.r-project.org/package=nseval)
[![Travis build status](http://travis-ci.org/crowding/nseval.svg?branch=master)](https://travis-ci.org/crowding/nseval)
[![Code coverage](https://codecov.io/gh/crowding/nseval/branch/master/graph/badge.svg)](https://codecov.io/gh/crowding/nseval)

`nseval` is the missing API for non-standard evaluation and
metaprogramming in R.

### Who NSEval is for

`nseval` might be for you if:

* You've been befuddled by trying to get your desired results using
  functions like `substitute`, `eval`, `parent.frame()`, `do.call`,
  `match.call`, and other cases of [non-standard evauation](http://adv-r.had.co.nz/Computing-on-the-language.html);
* You're been befuddled by trying to interface with other people's
  code that uses the above functions, and need a way to work around
  them;
* You want to better understand what "goes on" "under the hood" when R
  is running.

## Installation

```
install.packages("devtools")
library(devtools)
install_github("crowding/nseval")
```

### What `nseval` does

`nseval` introduces two S3 classes: `quotation`, and `dots`, which
mirror R's promises and `...`, respectively. Unlike their
counterparts, these are ordinary data objects, and can be assigned to
variables and be manipulated without triggering evaluation.

* A `quotation` combines an R expression with an environment.  There
  are also `forced` quotations, which pair an expression with a value.
* A `dots` is a named list of quotations.

There is a set of consistently-named accessors and constructors for
capturing, constructing, and manipulating these objects.

## Quick intro / transitioning from base R to NSEval

* Instead of `quote`, use:
  * `quo`. This captures the environment along with the text of its argument.
  * Similarly, `dots()` captures multiple arguments, analagously to `alist`.
* Instead of `substitute(x)`, use:
  * `arg(x)`, which captures the argument's environment along with its text.
* Instead of `substitute(list(...))[[2]]`, or `substitute(...())`, use:
    * `dots(...)`, to capture `...` unevaluated, including original environments, or
    * `arg_list(x, y, (...))`, to include other arguments as well.
* Instead of `match.call`, use:
    * `get_call`, which preserves the environment attached to each argument.
* Instead of `do.call`, use
  * `do`, which allows different arguments to be passed from
    different environments.
* Instead of `parent.frame`, use:
  * `arg_env`, which gives the environment _attached to an argument_, which
    is what you actually want most the time, or
  * `caller`, which returns the calling environment, like
    `parent.frame` often does, but avoids the latter's difficulties
    with lazy evaluation and closures; `caller` would rather throw an error
    than return an incorrect result.

### Why `nse` is needed

Before R, there was S, and S had some metaprogramming facilities,
exposed by functions like `parent.frame`, `substitute`, `match.call`,
`do.call`, `quote`, `alist`, `eval`, and so on. R duplicated that
API. But S did not have lexical scoping, closures, or the notion of an
environment, whereas R has all those things.

In S, lazily evaluated arguments could be evaluated simply by stepping
one step up in the call stack and evaluating them in that context.
This is not the case with R, because environments can come from
different sources via `...`, drop off the stack, and then be
re-activated via closures (and these situations happen frequently
enough in everyday code).

So R has been coping with a metaprogramming API that was not designed
with R's capabilities in mind. Because the S interface is not
sufficient to model R behavior, we end up with consequences such as:

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
equivalents from `nseval`, and may then have fewer of these kinds of
problems.

### What `nseval` doesn't do

`nseval` doesn't implement quasiquotation or hygeinic macros or code
coverage or DSLs or interactive debugging. But it is intended to be a
solid foundation to build those kinds of tools on! Watch this space.

`nseval` doesn't introduce any fancy syntax -- the only nonstandard
evaluation in its own interface is name lookup and quoting, and
standard-evaluating equivalents are always also present.

`nseval` doesn't try and remake all of R's base library, just the parts
about calls and lazy evaluation.

`nseval` has no install dependencies and should play well with base R or any
other 'verse.

## Similar packages

Some other packages have tread similar ground:

* [rlang](https://github.com/r-lib/rlang)
* [lazyeval](https://github.com/hadley/lazyeval)
* [pryr](https://github.com/hadley/pryr)
* [vadr](https://github.com/crowding/vadr), which this package is
  carved off and rewritten from.

## Further reading

It turns out that R's implementation of lazy evaluation via "promise"
objects amount to a recreation of
[fexprs](https://en.wikipedia.org/wiki/Fexpr). On the topic of how to
work with fexprs, particularly in combination with lexical scope and
environments, John Shutt's 2010
[PhD thesis](https://web.wpi.edu/Pubs/ETD/Available/etd-090110-124904/unrestricted/jshutt.pdf)
has been helpful.
