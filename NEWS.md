# nseval 0.4.2

Fixes:
* Fixed handling of missing arguments in `do` and `quo`
* `do` now handles arguments via a temporary binding of `...` in some
  cases. This should fix some cases where `sys.calls()` returned naked
  promises, which made for confusing debugging.
* Fixed man pages which were not generated completely.

Changes:
* Implemented `all.equal` method for `quotation` objects.
* dots and quotation objects now print themselves in a `deparse()`-like style.

# nseval 0.4.1

Fixes:
 * `caller` now correctly returns its `ifnotfound` argument.
 
# nseval 0.4

Initial CRAN Release.
