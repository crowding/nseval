#include "vadr.h"

/* Assert that some object is a type. */
/* void assert_type(SEXP x, SEXPTYPE type) { */
/*   if (TYPEOF(x) != type) { */
/*     error("Expected %s, got %s", type2char(type), type2char(TYPEOF(x))); */
/*   } */
/* } */

int is_language(SEXP x) {
  switch(TYPEOF(x)) {
  case LANGSXP:
  case SYMSXP:
  case PROMSXP:
  case DOTSXP:
    return 1;
  default:
    return 0;
  }
}

SEXP emptypromise(void) {
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
  if (is_language(value) && value != R_MissingArg) {
    SET_PRCODE(out, Rf_lang2(install("quote"), value));
  } else {
    SET_PRCODE(out, expr);
  }
  SET_PRENV(out, R_NilValue);
  SET_PRVALUE(out, value);
  UNPROTECT(1);
  return out;
}

SEXP new_weird_promise(SEXP expr, SEXP env, SEXP value) {
  SEXP out = PROTECT(allocSExp(PROMSXP));
  if (is_language(value) && value != R_MissingArg) {
    SET_PRCODE(out, Rf_lang2(install("quote"), value));
  } else {
    SET_PRCODE(out, expr);
  }
  SET_PRENV(out, env);
  SET_PRVALUE(out, value);
  UNPROTECT(1);
  return out;
}

#if R_VERSION < R_Version(4, 4, 1)
SEXP allocLang(int n)
{
  if (n > 0)
    return LCONS(R_NilValue, allocList(n - 1));
  else
    return R_NilValue;
}
#endif

#if R_VERSION < R_Version(4, 2, 0)
Rboolean R_existsVarInFrame(SEXP rho, SEXP symbol) {
  return (Rf_findVarInFrame3(rho, symbol, FALSE) != R_UnboundValue);
}
#endif
