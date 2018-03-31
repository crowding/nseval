#include "vadr.h"
#include "promises.h"

/* Immutable promise objects (quotations) are stored by this package as
   CLOSXPS with a class attribute.  For unforced quotations, the
   environment and body slots are filled, while the arg slot is
   empty. For forced quotations, the environment is set to R_EmptyEnv,
   and the body to the value.  In both cases the arglist is empty. */

SEXP _quotation(SEXP envir, SEXP expr, SEXP value) {
  SEXP out = PROTECT(allocSExp(CLOSXP));
  SET_FORMALS(out, R_NilValue);
  SEXP prom;
  
  if (envir == R_NilValue) {
    /* already-forced promise. Record a PROMSXP in the body. */
    prom = PROTECT(allocSExp(PROMSXP));
    SET_PRENV(prom, R_NilValue);
    SET_PRCODE(prom, expr);
    SET_PRVALUE(prom, value);
    SET_CLOENV(out, R_EmptyEnv);
    SET_BODY(out, prom);
    UNPROTECT(1);
  } else {
    assert_type(envir, ENVSXP);
    if (value != R_NilValue) {
      error("Can't make a promise with both an env and a value");
    } else {
      SET_CLOENV(out, envir);
      SET_BODY(out, expr);
    }
  }

  setAttrib(out, R_ClassSymbol, mkString("quotation"));

  UNPROTECT(1);
  return out;
}

/* Test if a quotation is "forced" */
int forced_quotation(SEXP clos) {
  return CLOENV(clos) == R_EmptyEnv && TYPEOF(BODY(clos)) == PROMSXP;
}

/* Test if a quotation is "forced" */
SEXP _forced_quotation(SEXP clos) {
  return ScalarLogical(forced_quotation(clos));
}

SEXP _expr_quotation(SEXP q) {
  if (forced_quotation(q)) {
    return PREXPR(BODY(q));
  } else {
    return BODY(q);
  }
}

SEXP promsxp_to_quotation(SEXP prom) {
  assert_type(prom, PROMSXP);
  SEXP out = PROTECT(allocSExp(CLOSXP));
  
  // if we have an unevluated promise whose code is another promise, descend
  while ((PRENV(prom) != R_NilValue) && (TYPEOF(PRCODE(prom)) == PROMSXP)) {
    prom = PRCODE(prom);
  }

  if (PRENV(prom) == R_NilValue) {
    /* forced promise. We'll put the original promise into the body. */
    SET_CLOENV(out, R_EmptyEnv);
    SET_BODY(out, prom);
    SET_FORMALS(out, R_NilValue);
  } else {
    SET_CLOENV(out, PRENV(prom));
    SET_BODY(out, PREXPR(prom));
    SET_FORMALS(out, R_NilValue);
  }
  setAttrib(out, R_ClassSymbol, mkString("quotation"));

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

SEXP _quotation_to_promsxp(SEXP clos) {
  assert_type(clos, CLOSXP);
  SEXP out;
  if (forced_quotation(clos)) {
    /* In the case of forced promises we return the same promsxp. */
    return BODY(clos);
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
SEXP make_into_promsxp(SEXP in) {
  if (TYPEOF(in) == PROMSXP) {
    while (TYPEOF(PREXPR(in)) == PROMSXP) {
      in = PREXPR(in);
    }
    return in;
  } else {
    /* wrap in a forced promise */
    SEXP out = PROTECT(allocSExp(PROMSXP));
    SET_PRENV(out, R_EmptyEnv);
    SET_PRVALUE(out, in);
    SET_PRCODE(out, in);
    UNPROTECT(1);
    return out;
  }
}

SEXP _quotation_literal(SEXP in) {
  SEXP pr = PROTECT(allocSExp(PROMSXP));
  SEXP fn = PROTECT(allocSExp(CLOSXP));
  
  /* wrap in a forced promise */
  SET_PRENV(pr, R_EmptyEnv);
  SET_PRVALUE(pr, in);
  SET_PRCODE(pr, in);

  SET_CLOENV(fn, R_EmptyEnv);
  SET_BODY(fn, pr);
  SET_FORMALS(fn, R_NilValue);

  setAttrib(fn, R_ClassSymbol, mkString("quotation"));
  UNPROTECT(2);

  return fn;
}
