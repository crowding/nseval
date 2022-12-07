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
