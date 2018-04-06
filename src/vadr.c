#include "vadr.h"

/* Assert that some object is a type. */
/* void assert_type(SEXP x, SEXPTYPE type) { */
/*   if (TYPEOF(x) != type) { */
/*     error("Expected %s, got %s", type2char(type), type2char(TYPEOF(x))); */
/*   } */
/* } */

/* As above, with extra words about what thing is to be an expected type */
void assert_type3(SEXP x, SEXPTYPE type, const char *what) {
  if (TYPEOF(x) != type) {
    error("Expected %s in %s, got %s",
          type2char(type), what, type2char(TYPEOF(x)));
  }
}

int recycle_length(int i, int j) {
  if (MIN(i,j) == 0) return 0;
  int n = MAX(i,j);
  if ((n%i != 0) || (n%j != 0)) {
    warning("Longer vector length is not a multiple of shorter vector length");
  }
  return n;
}

SEXP emptypromise() {
  SEXP out = PROTECT(allocSExp(PROMSXP));
  SET_PRCODE(out, R_MissingArg);
  SET_PRENV(out, R_EmptyEnv);
  SET_PRVALUE(out, R_UnboundValue);
  UNPROTECT(1);
  return out;
}

SEXP new_promise(SEXP expr, SEXP env) {
  SEXP out = PROTECT(allocSExp(PROMSXP));
  SET_PRCODE(out, expr);
  SET_PRENV(out, env);
  SET_PRVALUE(out, R_UnboundValue);
  UNPROTECT(1);
  return out;
}

SEXP new_forced_promise(SEXP expr, SEXP value) {
  SEXP out = PROTECT(allocSExp(PROMSXP));
  SET_PRCODE(out, expr);
  SET_PRENV(out, R_NilValue);
  SET_PRVALUE(out, value);
  UNPROTECT(1);
  return out;
}
