#include "vadr.h"
#include "promises.h"

/* Immutable promise objects (quotations) are represented as call objects,
   specifically `evalq(<expr>, <env>)` for unforced quotations,
   and `if (FALSE) expr else value` for forced quotations.

   Follows the rule for PROMSXPs:
   To make an unforced quotations, `envir` "should" be R_NilValue.
   To make a forced quotation, `value` should be R_UnboundValue.
     but R_Nilvalue or R_MissingValue will be accepted as long
     as `envir` is R_NilValue (since _quotation is .Call'ed from R level,
     and R_UnboundValues shouldn't manifest at R level.)

     unforced quotations are:
     evalq(<expr>, <env>)
     CAR(q) install("evalq")
     CADR(q) <expr>
     CADDR(q) <env>

     forced quotations are:
     in R:
     if(FALSE) <expr> else <value>
     if(FALSE) <expr> else quote(<value>)

     CAR(q) install("if")
     CADR(q) ScalarLogical(FALSE)
     CADDR(q) <expr>, but *possibly* unforced quotation
     CADDDR(q) <value> *or* LANGSXP, with:
     CAR(CADDDR(q)) install("quote")
     CADR(CADDDR(q)) <value>
*/

SEXP _quotation(SEXP envir, SEXP expr, SEXP value, SEXP sigil) {
  SEXP out;
  if (expr == R_MissingArg) {
    out = Rf_lang3(install("evalq"), expr, R_EmptyEnv);
  } else if (TYPEOF(envir) != ENVSXP) {
    if (value == sigil) {
      error("Quotations should have an environment OR a value (got neither)");
    } else if (is_language(value)) {
      SEXP q = PROTECT(Rf_lang2(install("quote"), value));
      out = Rf_lang4(install("if"),
                     ScalarLogical(FALSE),
                     expr,
                     q);
      UNPROTECT(1);
    } else {
      out = Rf_lang4(install("if"),
                     ScalarLogical(FALSE),
                     expr,
                     value);
    }
  } else {
    assert_type(envir, ENVSXP);
    if ((value != sigil) &&
        (value != R_UnboundValue)) {
      // Huh, so this actually happens during some primitive function's
      // generic dispatch. In particular "c" generic dispatch forces its
      // first arg then apparently puts the result into both value and expr,
      // while also leaving the env slot filled.
      SEXP x = PROTECT(Rf_lang3(install("evalq"), expr, envir));
      setAttrib(x, R_ClassSymbol, mkString("quotation"));
      if (is_language(value)) {
        SEXP q = PROTECT(Rf_lang2(install("quote"), value));
        out = Rf_lang4(install("if"),
                       ScalarLogical(FALSE),
                       x,
                       q);
        UNPROTECT(2);
      } else {
        out = Rf_lang4(install("if"),
                       ScalarLogical(FALSE),
                       x,
                       value);
        UNPROTECT(1);
      }        
    } else {
      out = Rf_lang3(install("evalq"), expr, envir);
    }
  }
  PROTECT(out);
  setAttrib(out, R_ClassSymbol, mkString("quotation"));
  UNPROTECT(1);
  return out;
}

int is_quotation(SEXP value) {
  const char *cp[] = {"quotation", ""};
  return R_check_class_etc(value, cp) != -1;  
}

int is_plausible_quotation(SEXP value) {
  return (TYPEOF(value) == LANGSXP
          && ((CAR(value) == install("if")
               && TYPEOF(CADR(value)) == LGLSXP
               && CADDDR(value) != R_NilValue
               ) || (CAR(value) == install("evalq")
                     && TYPEOF(CADDR(value)) == ENVSXP)));
}

SEXP _is_plausible_quotation(SEXP value) {
  return ScalarLogical(is_plausible_quotation(value));
}

SEXP _quotation_old(SEXP envir, SEXP expr, SEXP value) {
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
int is_forced_quotation(SEXP clos) {
  switch(TYPEOF(clos)) {
  case CLOSXP:
    return CLOENV(clos) == R_EmptyEnv && TYPEOF(BODY(clos)) == PROMSXP;
    break;
  case LANGSXP:
    return (CAR(clos) == install("if"));
    break;
  default: error("Unexpected sexptype in quotation");
  }
}

/* Test if a quotation is "forced" */
SEXP _is_forced_quotation(SEXP clos) {
  return ScalarLogical(is_forced_quotation(clos));
}

SEXP _expr_quotation(SEXP q) {
  switch(TYPEOF(q)) {
  case CLOSXP:
    if (is_forced_quotation(q)) {
      return PREXPR(BODY(q));
    } else {
      return BODY(q);
    }
    break;
  case LANGSXP:
    if (is_forced_quotation(q)) {
      if (is_quotation(CADDR(q))) {
        /* must be a quo of one of those weird primitive dispatch promises */
        return CADR(CADDR(q));
      } else {
        return CADDR(q);
      }
    } else {
      return CADR(q);
    }
    break;
  default: error("Unexpected sexptype in quotation");
  }
}

SEXP _env_quotation(SEXP q) {
  switch(TYPEOF(q)) {
  case CLOSXP:
    if (is_forced_quotation(q)) {
      return R_EmptyEnv;
    } else {
      return CLOENV(q);
    }
    break;
  case LANGSXP:
    if (is_forced_quotation(q)) {
      if (is_quotation(CADDR(q))) {
        /* must be a quo from one of those weird primitive dispatch promises */
        return CADDR(CADDR(q));
      } else {
        return R_EmptyEnv;
      }
    } else {
      return CADDR(q);
    }
    break;
  default: error("Unexpected sexptype in quotation");
  }
}

SEXP _value_quotation(SEXP q) {
  if (is_forced_quotation(q)) {
    switch(TYPEOF(q)) {
    case CLOSXP:
      /* we held a promsxp in the body */
      return PRVALUE(BODY(q));
    case LANGSXP: {
      SEXP x = CADDDR(q);
      if (is_language(x) && CAR(x) == R_QuoteSymbol) {
        x = CADR(x);
      }
      return(x);
    }
    default: error("Unexpected sexptype in quotation");
    }
  } else {
    error("Can't get value of unforced quotation");
  }
}

SEXP promsxp_to_quotation(SEXP prom) {
  // if we have an unevluated promise whose code is another promise, unwrap
  while ((TYPEOF(prom) == PROMSXP)
         && (PRENV(prom) != R_NilValue)
         && (TYPEOF(PRCODE(prom)) == PROMSXP)) {
    prom = PRCODE(prom);
  }

  if (TYPEOF(PRCODE(prom)) == BCODESXP) {
    return _quotation(PRENV(prom), R_BytecodeExpr(PRCODE(prom)),
                      PRVALUE(prom), R_UnboundValue);
  } else {
    return _quotation(PRENV(prom), PRCODE(prom),
                      PRVALUE(prom), R_UnboundValue);
  }
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
  if (_expr_quotation(clos) == R_MissingArg) {
    return R_MissingArg;
  } else if(is_forced_quotation(clos)) {
    return new_forced_promise(_expr_quotation(clos), _value_quotation(clos));
  } else {
    return new_promise(_expr_quotation(clos), _env_quotation(clos));
  }
}

SEXP forced_value_promise(SEXP in) {
  if (is_language(in)) {
    SEXP q = PROTECT(Rf_lang2(install("quote"), in));
    SEXP out = new_forced_promise(q, in);
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
    return forced_value_promise(in);
  }
}

SEXP _quotation_literal(SEXP in) {
  return promsxp_to_quotation(forced_value_promise(in));
}
