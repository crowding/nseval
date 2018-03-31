#include "vadr.h"
#include "promises.h"

SEXP _get_dots(SEXP env, SEXP inherit); /* in dots.c */
SEXP _quotation_literal(SEXP in); /* in promises.c */

static int ddVal(SEXP symbol)
{
  const char *buf = CHAR(PRINTNAME(symbol));
  if ( !strncmp(buf, "..", 2) && strlen(buf) > 2) {
    char *endp;
    int rval = strtol(buf+2, &endp, 10);
    if (*endp != '\0')
      return 0;
    else
      return rval;
  } else {
    return 0;
  }
}

SEXP _locate_all(SEXP sym, SEXP env, SEXP function) {
  return R_NilValue;
}

SEXP _locate(SEXP sym, SEXP env, SEXP function) {
  assert_type(sym, SYMSXP);
  assert_type(env, ENVSXP);
  int fn = asLogical(function);

  if (DDVAL(sym)) {
    error("locate_: double dot symbol `%s` not supported", CHAR(PRINTNAME(sym)));
  }

  while (env != R_EmptyEnv) {
    assert_type(env, ENVSXP);
    if (fn) {
      SEXP x = findVarInFrame3(env, sym, TRUE);
      if (TYPEOF(x) == PROMSXP) {
        if (PRVALUE(x) == R_UnboundValue) {
          /* Per R rules, we must force. As forcing isn't exposed in
             Rinternals, I'll do it by calling "force"... or forceAndCall and
             then calling force.... */
          SEXP force = findVarInFrame3(R_BaseNamespace, install("force"), TRUE);
          SEXP callForce = PROTECT(list2(force, sym));
          R_forceAndCall(callForce, 1, env);
          UNPROTECT(1);
          if (PRVALUE(x) == R_UnboundValue) {
            error("forcing failed???");
          }
          x = PRVALUE(x);
        }
      }

      switch(TYPEOF(x)) {
      case CLOSXP: 
      case SPECIALSXP:
      case BUILTINSXP:
        return env;
      default:
        break;
      }
    } else {
      SEXP x = findVarInFrame3(env, sym, FALSE);

      if (x != R_UnboundValue) {
        return env;
      }
    }
    env = ENCLOS(env);
  }
  return R_NilValue;
}

SEXP do_ddfindVar(SEXP symbol, SEXP envir) {
  int i = ddVal(symbol);
  SEXP vl = _get_dots(envir, ScalarLogical(TRUE));
  if (vl != R_NilValue) {
    if (length(vl) >= i) {
      vl = nthcdr(vl, i - 1);
      return(CAR(vl));
    }
    else
      error("the ... list does not contain %d elements", i);
  }
  else error("..%d used in an incorrect context, no ... to look in", i);

  return R_NilValue;
}

SEXP x_findVar(SEXP sym, SEXP envir) {
  assert_type(sym, SYMSXP);
  assert_type(envir, ENVSXP);
  SEXP binding;
  if (DDVAL(sym)) {
    binding = do_ddfindVar(sym, envir);
  } else {
    binding = Rf_findVar(sym, envir);
  }
  if (TYPEOF(binding) == PROMSXP) { 
    while(TYPEOF(PREXPR(binding)) == PROMSXP) {
      binding = PREXPR(binding);
    }
  }
  return binding;
}

SEXP do_findBinding(SEXP sym, SEXP envir) {
  SEXP binding = x_findVar(sym, envir);
  if (binding == R_UnboundValue) {
    error("Variable `%s` was not found.",
          CHAR(PRINTNAME(sym)));
  }
  return binding;
}


SEXP do_findPromise(SEXP name, SEXP envir) {
  SEXP binding = do_findBinding(name, envir);
  if (binding == R_MissingArg) {
    binding = emptypromise();
  }
  if (TYPEOF(binding) != PROMSXP) {
    error("Variable `%s` was not bound to a promise",
          CHAR(PRINTNAME(name)));
  }
  return binding;
}

int unwrappable(SEXP prom) {
  while(TYPEOF(PREXPR(prom)) == PROMSXP) {
    prom = PREXPR(prom);
  }
  return (TYPEOF(PREXPR(prom)) == SYMSXP
          && PRENV(prom) != R_NilValue
          && PRENV(prom) != R_EmptyEnv);
}

SEXP unwrap_promise(SEXP prom, int recursive) {
  while(unwrappable(prom)) {
    SEXP name = PREXPR(prom);
    SEXP env = PRENV(prom);
    SEXP binding = x_findVar(name, env);
    /* Rprintf("env(%p) is %p where %s -> %p which is a %s\n", */
    /*         prom, */
    /*         env, */
    /*         CHAR(PRINTNAME(name)), */
    /*         binding, */
    /*         type2char(TYPEOF(binding))); */
    if (binding == R_MissingArg) {
      return emptypromise();
    } else if (binding == R_UnboundValue) {
      break;
    } else if (TYPEOF(binding) == PROMSXP) {
      prom = binding;
    } else {
      return binding;
    }
    if (!recursive) {
      break;
    };
  }
  return prom;
}

SEXP _unwrap_quotation(SEXP q, SEXP recursive) {
  SEXP pr = PROTECT(_quotation_to_promsxp(q));
  pr = PROTECT(unwrap_promise(pr, asLogical(recursive)));
  if (TYPEOF(pr) == PROMSXP) {
    q = promsxp_to_quotation(pr);
  } else {
    q = _quotation_literal(pr);
  }
  UNPROTECT(2);
  return q;
}

/* selector for things arg_get finds */
typedef enum GET_ENUM {
  EXPR,
  ENV,
  PROMISE,
  IS_LITERAL, /* not a "test" because it follows logic of inspecting expr/value */
  IS_MISSING
} GET_ENUM;

const char* get_enum_string(GET_ENUM type) {
  switch(type) {
  case EXPR: return "expression";
  case ENV: return "environment";
  case PROMISE: return "promise";
  case IS_LITERAL: return "is literal";
  case IS_MISSING: return "is missing";
  }
}

/* selector for things arg_check finds */
typedef enum TEST_ENUM {
  IS_PROMISE,
  IS_FORCED
} TEST_ENUM;

const char* test_enum_string(TEST_ENUM type) {
  switch(type) {
  case IS_PROMISE: return "is promise";
  case IS_FORCED: return "is forced";
  }
}

SEXP arg_get(SEXP, SEXP, GET_ENUM, int, int);

SEXP arg_get_from_unforced_promise(SEXP prom, GET_ENUM request, int warn) {
  SEXP expr = PREXPR(prom);
  switch(request) {
  case EXPR:
    return PREXPR(prom);
  case ENV:
    return PRENV(prom);
  case PROMISE:
    return prom;
  case IS_LITERAL:
    switch(TYPEOF(expr)) {
    case INTSXP:
    case STRSXP:
    case REALSXP:
      if (LENGTH(expr) > 1 || ATTRIB(expr) != R_NilValue) {
        return ScalarLogical(FALSE);
      } else {
        return ScalarLogical(TRUE);
      }
    default: 
      return ScalarLogical(FALSE);
    }
  case IS_MISSING:
    // unwrap.....
    {
      SEXP unwrapped = unwrap_promise(prom, TRUE);
      if (PREXPR(prom) == R_MissingArg) {
        return ScalarLogical(TRUE);
      } else {
        return ScalarLogical(FALSE);
      }
    }
  }
}

SEXP arg_get_from_forced_promise(SEXP sym, SEXP prom, GET_ENUM request, int warn) {
  SEXP expr = PREXPR(prom);

  switch(request){
  case EXPR:
  case ENV:
  case PROMISE:
    break;
  case IS_LITERAL:
  case IS_MISSING:
    warn = 0; /* don't warn for true/false checks */
  }

  switch(TYPEOF(expr)) {
  case INTSXP:                  /* plausible code literals */
  case STRSXP:
  case REALSXP:
    switch (request) {
    case EXPR:
      return expr;
    case ENV:
      if (warn) {
        if (LENGTH(expr) > 1 || ATTRIB(expr) != R_NilValue) {
          warning("Argument `%s` forced from non-literal %s; can't find environment",
                  CHAR(PRINTNAME(sym)),
                  type2char(TYPEOF(expr)));      
        }
      }
      return R_EmptyEnv;
    case PROMISE: 
      return prom;
    case IS_LITERAL: 
      if (LENGTH(expr) > 1 || ATTRIB(expr) != R_NilValue) {
        return ScalarLogical(FALSE);
      } else {
        return ScalarLogical(TRUE);
      }
    case IS_MISSING: 
      return ScalarLogical(FALSE);
    }
  case SYMSXP:                  /* can't fake an environment we don't have
                                   if the expr is not literal */
    switch(request) {
    case ENV: 
      if(warn) warning("Argument `%s` already forced so cannot determine environment.",
                       CHAR(PRINTNAME(sym)));
      return R_EmptyEnv;
    case PROMISE: 
      return prom;
    case EXPR: 
      return expr;
    case IS_LITERAL: 
      /* the missing is a kind of literal */
      return ScalarLogical(expr == R_MissingArg);
    case IS_MISSING:
      /* Forced, so no need to unwrap */
      if (PREXPR(prom) == R_MissingArg) {
        return ScalarLogical(TRUE);
      } else if (PRVALUE(prom) == R_MissingArg) {
        return ScalarLogical(TRUE);
      } else {
        return ScalarLogical(FALSE);
      }
    }
  case LANGSXP:
    switch(request) {
    case ENV:
      if(warn) {
        warning("Argument `%s` already forced so cannot determine environment.",
                CHAR(PRINTNAME(sym)));
      };
      return R_EmptyEnv;
    case PROMISE:
      return prom;
    case EXPR:
      return expr;
    case IS_LITERAL:
      return ScalarLogical(FALSE);
    case IS_MISSING:
      return ScalarLogical(PRVALUE(prom) == R_MissingArg);
    }
  default:
    switch(request) {
    case ENV:
      if(warn) warning("Argument `%s` already forced, %s found instead of expression?",
                       CHAR(PRINTNAME(sym)), type2char(TYPEOF(expr)));
      return R_EmptyEnv;
    case EXPR:
      return expr;
    case PROMISE:
      return prom;
    case IS_LITERAL:
      return ScalarLogical(FALSE);
    case IS_MISSING:
      return ScalarLogical(FALSE);
    }
  }
}

SEXP arg_get_from_nonpromise(SEXP sym, SEXP value, GET_ENUM request, int warn) {
  SEXP expr;
  SEXP prom;

  switch(TYPEOF(value)) {
  case INTSXP:                  /* plausible code literals */
  case STRSXP:
  case REALSXP:

    switch(request) {
    case EXPR: 
      if (LENGTH(value) > 1 || ATTRIB(value) != R_NilValue) {
        if(warn) warning("`%s` not a promise, bound to non-scalar %s instead.",
                         CHAR(PRINTNAME(sym)),
                         type2char(TYPEOF(value)));
      }
      return value;
    case ENV: 
      return R_EmptyEnv;
    case PROMISE: 
      return new_forced_promise(value, value);
    case IS_LITERAL: 
      return ScalarLogical(LENGTH(value) == 1 && ATTRIB(value) == R_NilValue);
    case IS_MISSING: 
      return ScalarLogical(FALSE);
    }
  case SYMSXP:
    if (value == R_UnboundValue) {
      error("Variable `%s` was not found.",
            CHAR(PRINTNAME(sym)));
    } else if (value == R_MissingArg) {
      switch (request) {
      case EXPR:
        return R_MissingArg;
      case ENV:
        return R_EmptyEnv;
      case PROMISE:
        return emptypromise();
      case IS_LITERAL:
        /* missing is a literal, in that the bytecompiler inlines it */
        return ScalarLogical(TRUE);
      case IS_MISSING:
        return ScalarLogical(TRUE);
      }
    } else { /* a non missing symbol */
      if(warn) warning("`%s` not a promise, contains symbol `%s`",
                       CHAR(PRINTNAME(sym)),
                       CHAR(PRINTNAME(value)));
      switch(request) {
      case ENV:
        if(warn) warning("`%s` not a promise, contains symbol `%s`",
                         CHAR(PRINTNAME(sym)),
                         CHAR(PRINTNAME(value)));
        return R_EmptyEnv;
      case EXPR:
        if(warn) warning("`%s` not a promise, contains symbol `%s`",
                         CHAR(PRINTNAME(sym)),
                         CHAR(PRINTNAME(value)));
        /* we are now making up `quote(x)` as a plausible way of
           having got the symbol `x`. However since we also return
           EmptyEnv this should produce an error if the user attempts
           to re-call. */
        return Rf_lang2(install("quote"), value);
      case PROMISE:
        if(warn) warning("`%s` not a promise, contains symbol `%s`",
                         CHAR(PRINTNAME(sym)),
                         CHAR(PRINTNAME(value)));
        expr = PROTECT(Rf_lang2(install("quote"), value));
        prom = new_forced_promise(expr, value);
        UNPROTECT(1);
        return prom;
      case IS_LITERAL:
        return ScalarLogical(FALSE);
      case IS_MISSING:
        return ScalarLogical(FALSE);        
      }
    }
  case LANGSXP:
    switch(request) {
    case EXPR:
      if(warn) warning("`%s` not a promise, contains a %s.",
                       CHAR(PRINTNAME(sym)),
                       type2char(TYPEOF(value)));
      return Rf_lang2(install("quote"), value);
    case ENV:
      if(warn) warning("`%s` not a promise, contains a %s.",
                       CHAR(PRINTNAME(sym)),
                       type2char(TYPEOF(value)));
      return R_EmptyEnv;
    case PROMISE:
      if(warn) warning("`%s` not a promise, contains a %s.",
                       CHAR(PRINTNAME(sym)),
                       type2char(TYPEOF(value)));
      expr = PROTECT(Rf_lang2(install("quote"), value));
      prom = new_forced_promise(expr, value);
      UNPROTECT(1);
      return prom;
    case IS_LITERAL: return ScalarLogical(FALSE);
    case IS_MISSING: return ScalarLogical(FALSE);
    }
  default:
    switch(request) {
    case ENV:
      if(warn) warning("`%s` not a promise, contains a %s.",
                       CHAR(PRINTNAME(sym)),
                       type2char(TYPEOF(value)));
      return R_EmptyEnv;
    case EXPR:
      if(warn) warning("`%s` not a promise, contains a %s.",
                       CHAR(PRINTNAME(sym)),
                       type2char(TYPEOF(value)));
      return value;
    case PROMISE:
      if(warn) warning("`%s` not a promise, contains a %s.",
                       CHAR(PRINTNAME(sym)),
                       type2char(TYPEOF(value)));
      return new_forced_promise(value, value);
    case IS_LITERAL:
      return ScalarLogical(FALSE);
    case IS_MISSING:
      return ScalarLogical(FALSE);
    }
  }
}

SEXP arg_get(SEXP envir, SEXP name, GET_ENUM type, int warn, int recursive) {
  assert_type(envir, ENVSXP);
  assert_type(name, SYMSXP);
  if (name == R_DotsSymbol) {
    error("Inappropriate use of ... in arg_*");    
  }
  /* Rprintf("Getting %s of binding `%s` in env `%p`\n", */
  /*         get_enum_string(type), CHAR(PRINTNAME(name)), envir); */
  SEXP binding = x_findVar(name, envir);
  if (TYPEOF(binding) == PROMSXP) {
    if (recursive) binding = unwrap_promise(binding, recursive);
    /* Rprintf("Got a promise\n"); */
    while (TYPEOF(PREXPR(binding)) == PROMSXP) {
      /* Rprintf("It's a wrapped promise\n"); */
      binding = PREXPR(binding);
    }
    if (PRVALUE(binding) != R_UnboundValue) {
      /* Rprintf("It's already forced\n"); */
      return arg_get_from_forced_promise(name, binding, type, warn);
    } else {
      /* Rprintf("It's unforced\n"); */
      return arg_get_from_unforced_promise(binding, type, warn);
    }
  } else {
    /* Rprintf("It's not a promise\n"); */
    return arg_get_from_nonpromise(name, binding, type, warn);
  }
}

SEXP arg_check(SEXP envir, SEXP name, TEST_ENUM query, int warn) {
  assert_type(envir, ENVSXP);
  assert_type(name, SYMSXP);
  if (name == R_DotsSymbol) {error("Inappropriate use of ... in arg_*");}
  SEXP binding = do_findBinding(name, envir);
  while (TYPEOF(binding) == PROMSXP && TYPEOF(PREXPR(binding)) == PROMSXP) {
    /* Rprintf("Got a wrapped promise\n"); */
    binding = PREXPR(binding);
  }
  switch(TYPEOF(binding)) {
  case PROMSXP:
    switch (query) {
    case IS_PROMISE: 
      return ScalarLogical(TRUE);
    case IS_FORCED: 
      if (PRENV(binding) == NULL) {
        /* Rprintf("Argument %s env is null\n", */
        /*         CHAR(PRINTNAME(name))); */
        return ScalarLogical(TRUE);
      } else if (PRVALUE(binding) == R_UnboundValue) {
        /* Rprintf("Argument %s value slot is unbound\n", */
        /*         CHAR(PRINTNAME(name))); */
        return ScalarLogical(FALSE);
      } else if (PRVALUE(binding) == R_MissingArg) {
        /* Rprintf("Argument %s value slot is missing\n", */
        /*         CHAR(PRINTNAME(name))); */
        return ScalarLogical(FALSE);
      } else {
        /* Rprintf("Argument %s none of the above\n", */
        /*         CHAR(PRINTNAME(name))); */
        return ScalarLogical(TRUE);
      }
    }
  case SYMSXP:
    switch(query) {
    case IS_PROMISE:
      return ScalarLogical(FALSE);
    case IS_FORCED:
      /* missings are unforced by definition (and because compiler
         optimizes missing promsxps into missings */
      return ScalarLogical(binding != R_MissingArg);
    }
  default: 
    switch(query) {
    case IS_PROMISE:
      return ScalarLogical(FALSE);
    case IS_FORCED:
      return ScalarLogical(TRUE);
    }
  }
}

SEXP _arg_env(SEXP envir, SEXP name, SEXP warn) {
  return arg_get(envir, name, ENV, asLogical(warn), FALSE);
}

SEXP _arg_expr(SEXP envir, SEXP name, SEXP warn) {
  return arg_get(envir, name, EXPR, asLogical(warn), FALSE);
}

SEXP _arg_dots(SEXP envirs, SEXP names, SEXP tags, SEXP warn) {
  assert_type(envirs, VECSXP);
  assert_type(names, VECSXP);
  if (tags != R_NilValue) {
    assert_type(tags, STRSXP);
    if (LENGTH(tags) != LENGTH(names)) {
      error("Inputs to arg_dots have different lengths");
    }
  }
  if (LENGTH(envirs) != LENGTH(names)) {
    error("Inputs to arg_dots have different lengths");
  }
  int warni = asLogical(warn);

  int len = LENGTH(names);
  if (len == 0) {
    return R_NilValue;
  }
  /* so at least one item */
  SEXP head = R_NilValue;
  SEXP tail = head;
  for (int i = 0; i < len; i++) {
    if (VECTOR_ELT(names, i) == R_DotsSymbol) {
      SEXP dots = _get_dots(VECTOR_ELT(envirs, i), ScalarLogical(TRUE));
      for (SEXP j = dots; j != R_NilValue; j = CDR(j)) {
        if (head == R_NilValue) {
          head = PROTECT(allocSExp(DOTSXP)); /* we do this exactly once */
          tail = head;
        } else {
          SETCDR(tail, allocSExp(DOTSXP));
          tail = CDR(tail);
        }
        SETCAR(tail, CAR(j));
        SET_TAG(tail, TAG(j));
      }
    } else {
      if (head == R_NilValue) {
        head = PROTECT(allocSExp(DOTSXP)); /* we do this exactly once */
        tail = head;
      } else {
        SETCDR(tail, allocSExp(DOTSXP));
        tail = CDR(tail);
      }
      SEXP promise = 
        arg_get(VECTOR_ELT(envirs, i), VECTOR_ELT(names, i), 
                PROMISE, asLogical(warn), FALSE);
      SETCAR(tail, promise);
      if (tags == R_NilValue) {
        SEXP name = VECTOR_ELT(names, i);
        assert_type(name, SYMSXP); 
        SET_TAG(tail, name);
      } else {
        SEXP name = STRING_ELT(tags, i);
        if (name != R_BlankString) {
          name = installChar(name);
          SET_TAG(tail, name);
        } else {
          SET_TAG(tail, R_NilValue);
        }
      }
    }
  }
  SETCDR(tail, R_NilValue);
  UNPROTECT(1);
  return(head);
}

SEXP _arg(SEXP envir, SEXP name, SEXP warn) {
  SEXP prom = PROTECT(arg_get(envir, name, PROMISE, asLogical(warn), FALSE));
  SEXP retval = promsxp_to_quotation(prom);
  UNPROTECT(1);
  return retval;
}

SEXP _is_promise(SEXP envir, SEXP name, SEXP warn) {
  return arg_check(envir, name, IS_PROMISE, asLogical(warn));
}

SEXP _is_forced(SEXP envir, SEXP name, SEXP warn) {
  return arg_check(envir, name, IS_FORCED, asLogical(warn));
}

SEXP _is_literal(SEXP envir, SEXP name, SEXP warn) {
  return arg_get(envir, name, IS_LITERAL, asLogical(warn), FALSE);
}

SEXP _is_missing(SEXP envir, SEXP name, SEXP warn, SEXP recursive) {
  return arg_get(envir, name, IS_MISSING, asLogical(warn), asLogical(recursive));
}
