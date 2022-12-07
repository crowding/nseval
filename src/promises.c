#include "vadr.h"
#include "promises.h"

/* Immutable promise objects (quotations) are stored as CLOSXPS with a
   class attribute.  For unforced quotations, the environment and body
   slots are filled, while the arg slot is empty. For forced
   quotations, the environment is set to R_EmptyEnv, and the body to
   the value.  In both cases the arglist is empty. */

SEXP _quotation(SEXP envir, SEXP expr, SEXP value) {
  SEXP out = PROTECT(allocSExp(CLOSXP));
  SET_FORMALS(out, R_NilValue);
  SEXP prom;
  if (expr == R_MissingArg) {
    /* Ignore the environment. */
    SET_CLOENV(out, R_EmptyEnv);
    SET_BODY(out, expr);
  } else if (envir == R_NilValue) {
    /* already-forced promise. Record a PROMSXP in the body? */
    prom = PROTECT(new_forced_promise(expr, value));
    SET_CLOENV(out, R_EmptyEnv);
    SET_BODY(out, prom);
    UNPROTECT(1);
  } else {
    assert_type(envir, ENVSXP);
    if (value != R_MissingArg) {
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
  // if we have an unevluated promise whose code is another promise, unwrap
  while ((TYPEOF(prom) == PROMSXP)
         && (PRENV(prom) != R_NilValue)
         && (TYPEOF(PRCODE(prom)) == PROMSXP)) {
    prom = PRCODE(prom);
  }
  SEXP out;

  if (prom == R_MissingArg) {
    /* missing args are represented directly */
    out = PROTECT(allocSExp(CLOSXP));
    SET_BODY(out, prom);
    SET_CLOENV(out, R_EmptyEnv);
    SET_FORMALS(out, R_NilValue);
  } else {
    out = PROTECT(allocSExp(CLOSXP));
    assert_type(prom, PROMSXP);
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
  }
  setAttrib(out, R_ClassSymbol, mkString("quotation"));

  UNPROTECT(1);
  return out;
}

SEXP empty_closure(void) {
  SEXP out = PROTECT(allocSExp(CLOSXP));
  SET_FORMALS(out, R_NilValue);
  SET_BODY(out, R_MissingArg);
  SET_CLOENV(out, R_EmptyEnv);
  UNPROTECT(1);
  return out;
}

SEXP _quotation_to_promsxp(SEXP clos) {
  assert_type(clos, CLOSXP);
  if (forced_quotation(clos)) {
    /* In the case of forced promises we return the promsxp, which is
       stored in the body */
    return BODY(clos);
  } else if (BODY(clos) == R_MissingArg) {
    return R_MissingArg;
  } else {
    return new_promise(BODY(clos), CLOENV(clos)); 
  }
}

SEXP forced_promise(SEXP in) {
  if (is_language(in)) {
    SEXP q = PROTECT(Rf_lang2(install("quote"), in));
    SEXP out =  new_forced_promise(q, in);
    UNPROTECT(1);
    return out;
  } else {
    return new_forced_promise(in, in);
  }
}

/* If not a promise, wrap in a promise. */
SEXP make_into_promsxp(SEXP in) {
  if (TYPEOF(in) == PROMSXP) {
    while (TYPEOF(PREXPR(in)) == PROMSXP) {
      in = PREXPR(in);
    }
    return in;
  } else {
    PROTECT(in);
    SEXP out = forced_promise(in);
    UNPROTECT(1);
    return out;
  }
}

SEXP _quotation_literal(SEXP in) {
  SEXP pr = PROTECT(forced_promise(in));
  SEXP fn = PROTECT(allocSExp(CLOSXP));
  
  SET_CLOENV(fn, R_EmptyEnv);
  SET_BODY(fn, pr);
  SET_FORMALS(fn, R_NilValue);
  setAttrib(fn, R_ClassSymbol, mkString("quotation"));
  
  UNPROTECT(2);

  return fn;
}
