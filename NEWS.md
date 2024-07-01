# nseval 0.5.2 (release date: 2023-07-01)

### Fixes:

* In preparation for normalization of an official R API, references to non-API functions `DDVAL`, `Rf_findVarInFrame3`, `SET_TYPEOF`, `SET_BODY`, `SET_FORMALS` and `SET_CLOENV` have been removed. The calls to PROMSXP accessor functions remain, as they are central to this package's purpose and it is not yet clear what the official API will have in their place.

# nseval 0.5.1 (release date: 2023-12-11)

### Fixes:

* Fix issues raised by CRAN:
  * Always cast argument of `printf("%p", ...)` to `(void *)`

# nseval 0.5

### Changes

* The representation of quotations has been changed. Quotations are now represented as expressions with a class attribute. This means that you can `eval` a quotation and it will return the same value that a promise would return when forced. This also means that you can `bquote` or `substitute` to place a quotation into an expression, and the resulting expression will evaluate _hygienically_.
* The better to support hygiene, `arg*` will check if there a quotation in the argument expression (as it would be if the call had been done with `bquote`) and transparently unwrap it.
* Added method `is_forced_.name`.
* Added `arg_value` to peek at values of promises (without forcing).
* Added `as.quo` conversion methods for formulas and `rlang` quosures.

### Fixes:

* Added some support for "weird" promises. These are created by primitive s3 methods dispatched from a generic like `c`. These promises are forced (have a value, which was dispatched from) but still have an environment.

# nseval 0.4.3

### Fixes

* Added memory protection calls where identified by `rchk`.
* Comply with `-Wstrict-prototypes`.

# nseval 0.4.2

### Changes

* Implemented `all.equal` method for `quotation` objects.
* `dots` and `quotation` objects now print themselves in a `deparse()`-like style.
* `is_*_()` functions how have methods for promises/dots.
* `set_` and `set_enclos_` are added to help with making assignments.

### Fixes

* Fixed handling of missing arguments in `do` and `quo`.
* `do` now passes arguments via a temporary binding of `...` in some
  cases. This should fix some cases where `sys.calls()` returned naked
  promises, which made for confusing debugging. For R versions before 
  4.0 the old behavior is retained.
* Fixed man pages which were not generated completely.
* `locate(fn, mode="function")` now works in the case where `fn` is
  a forced promise containing a function.


# nseval 0.4.1

### Fixes
 * `caller` now correctly returns its `ifnotfound` argument.
 
# nseval 0.4

Initial CRAN Release.
