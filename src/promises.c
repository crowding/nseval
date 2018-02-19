#include "vadr.h"
#include "promises.h"

int _is_forced_f(SEXP clos) {
  return CLOENV(clos) == R_EmptyEnv && TYPEOF(BODY(clos)) == PROMSXP;
}

SEXP promsxp_to_closxp(SEXP prom) {
  assert_type(prom, PROMSXP);
  SEXP out = PROTECT(allocSExp(CLOSXP));
  
  // if we have an unevluated promise whose code is another promise, descend
  while ((PRENV(prom) != R_NilValue) && (TYPEOF(PRCODE(prom)) == PROMSXP)) {
    prom = PRCODE(prom);
  }

  if (PRENV(prom) == R_NilValue) {
    /* forced promise. We'll just stuff it in the body for now? */
    SET_CLOENV(out, R_EmptyEnv);
    SET_BODY(out, prom);
    SET_FORMALS(out, R_NilValue);
  } else {
    SET_CLOENV(out, PRENV(prom));
    SET_BODY(out, PREXPR(prom));
    SET_FORMALS(out, R_NilValue);
  }
  setAttrib(out, R_ClassSymbol, mkString("promise"));

  UNPROTECT(1);
  return out;
}

SEXP empty_closure() {
  SEXP out = PROTECT(allocSExp(CLOSXP));
  SET_FORMALS(out, R_NilValue);
  SET_BODY(out, R_MissingArg);
  SET_CLOENV(out, R_EmptyEnv);
  UNPROTECT(1);
  return out;
}

SEXP closxp_to_promsxp(SEXP clos) {
  assert_type(clos, CLOSXP);
  SEXP out;
  if (_is_forced_f(clos)) {
    /* forced promise. in the case of forced promises we just unwrap
       the promsxp. */
    return BODY(clos);
    SET_PRVALUE(out, BODY(clos));
    SET_PRCODE(out, BODY(clos));
    SET_PRENV(out, R_NilValue);
  } else {
    out = PROTECT(allocSExp(PROMSXP));
    SET_PRVALUE(out, R_UnboundValue);
    SET_PRCODE(out, BODY(clos));
    SET_PRENV(out, CLOENV(clos));
  }
  UNPROTECT(1);
  return out;
}


/* If not a promise, wrap in a promise. */
SEXP make_into_promise(SEXP in) {
  if (TYPEOF(in) == PROMSXP) {
    while (TYPEOF(PREXPR(in)) == PROMSXP) {
      in = PREXPR(in);
    }
    return in;
  } else {
    /* wrap in a promise */
    SEXP out = PROTECT(allocSExp(PROMSXP));
    SET_PRENV(out, R_EmptyEnv);
    SET_PRVALUE(out, in);
    SET_PRCODE(out, in);
    UNPROTECT(1);
    return out;
  }
}
