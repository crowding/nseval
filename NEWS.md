# nseval 0.5

# Changes

* Representation of quotations has been changed.
* Added method `is_forced_.name`.
* Added `arg_value` to peek at values of promises (without forcing).
* Added `as.quo` conversion methods for formulas and `rlang` quosures.

### Fixes:

* Added some exceptions for "weird" promises found in s3 methods dispatched from a generic like `c`. These promises are forced (have a value, which was dispatched from) but still have their environments.

# nseval 0.4.3

Fixes:
* Added memory protection calls where identified by `rchk`.
* Comply with `-Wstrict-prototypes`.

# nseval 0.4.2

Changes:
* Implemented `all.equal` method for `quotation` objects.
* `dots` and `quotation` objects now print themselves in a `deparse()`-like style.
* `is_*_()` functions how have methods for promises/dots.
* `set_` and `set_enclos_` are added to help with making assignments.

Fixes:
* Fixed handling of missing arguments in `do` and `quo`.
* `do` now passes arguments via a temporary binding of `...` in some
  cases. This should fix some cases where `sys.calls()` returned naked
  promises, which made for confusing debugging. For R versions before 
  4.0 the old behavior is retained.
* Fixed man pages which were not generated completely.
* `locate(fn, mode="function")` now works in the case where `fn` is
  a forced promise containing a function.


# nseval 0.4.1

Fixes:
 * `caller` now correctly returns its `ifnotfound` argument.
 
# nseval 0.4

Initial CRAN Release.
