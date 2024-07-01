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

SEXP _locate(SEXP sym, SEXP env, SEXP function) {
  assert_type(sym, SYMSXP);
  assert_type(env, ENVSXP);
  Rboolean fn = asLogical(function);

  if (ddVal(sym)) {
    error("locate_: double dot symbol `%s` not supported", CHAR(PRINTNAME(sym)));
  }

  while (env != R_EmptyEnv) {
    assert_type(env, ENVSXP);
    LOG("looking in env %p for %s", (void *) env, CHAR(PRINTNAME(sym)));
    if (fn) {
      SEXP x = PROTECT(findVarInFrame(env, sym));
      LOG("got a %s", type2char(TYPEOF(x)));
      while (TYPEOF(x) == PROMSXP) {
        if (PRVALUE(x) == R_UnboundValue) {
          /* Per R rules, we must force. As forcing isn't exposed in
             Rinternals, I'll do it by calling "force"... or forceAndCall and
             then calling force.... */
          LOG("forcing it");
          SEXP force = findVarInFrame(R_BaseNamespace, install("force"));
          SEXP callForce = PROTECT(list2(force, sym));
          R_forceAndCall(callForce, 1, env);
          UNPROTECT(1);
        }
        x = PRVALUE(x);
        LOG("containing a %s", type2char(TYPEOF(x)));
      }

      switch(TYPEOF(x)) {
      case CLOSXP: 
      case SPECIALSXP:
      case BUILTINSXP:
        LOG("found a %s", type2char(TYPEOF(x)));
        UNPROTECT(1);
        return env;
      default:
        UNPROTECT(1);
        break;
      }
    } else {
      if (R_existsVarInFrame(env, sym)) {
        return env;
      }
    }
    env = R_ParentEnv(env);
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
  if (ddVal(sym)) {
    binding = do_ddfindVar(sym, envir);
  } else {
    binding = Rf_findVar(sym, envir);
  }
  if (TYPEOF(binding) == PROMSXP) { //TODO: unwrap here? (no?) 
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

int unwrappable(SEXP prom) {
  while(TYPEOF(PREXPR(prom)) == PROMSXP) {
    prom = PREXPR(prom);
  }
  int unwrappable =  (   TYPEOF(PREXPR(prom)) == SYMSXP
                      && PRENV(prom) != R_NilValue 
                      && PRENV(prom) != R_EmptyEnv);
  return unwrappable;
}

int is_forced_promise(SEXP prom) {
  return ( PRVALUE(prom) != R_UnboundValue
           || TYPEOF(PRENV(prom)) != ENVSXP);
}

SEXP unwrap_step(SEXP prom) {
  // attmpt to unwrap, returning another promise if one is found.
  // If no other promise is found return R_UnboundValue.
  while(TYPEOF(PREXPR(prom)) == PROMSXP) {
    prom = PREXPR(prom);
  }
  SEXP name = PREXPR(prom);
  SEXP env = PRENV(prom);
  SEXP binding = x_findVar(name, env);
  /* Rprintf("env(%p) is %p where %s -> %p which is a %s\n", */
  /*         (void *) prom, */
  /*         (void *) env, */
  /*         CHAR(PRINTNAME(name)), */
  /*         (void *) binding, */
  /*         type2char(TYPEOF(binding))); */
  if (binding == R_MissingArg) {
    return emptypromise();
  } else if (TYPEOF(binding) != PROMSXP) {
    return R_UnboundValue;
  } else {
    return binding; // possibly R_UnboundValue
  }
}

SEXP unwrap_promise(SEXP prom, int recursive) {
  SEXP tortoise = prom;
  SEXP next;
  PROTECT_INDEX prom_idx;
  PROTECT_INDEX tortoise_idx;
  PROTECT_WITH_INDEX(prom, &prom_idx);
  PROTECT_WITH_INDEX(tortoise, &tortoise_idx);
  
  while(TRUE) {

    if (!unwrappable(prom)) {
      UNPROTECT(2);
      return prom;
    }
    next = unwrap_step(prom);

    if (next == R_UnboundValue) {
      break;
    } else {
      prom = next;
      REPROTECT(prom, prom_idx);
    }

    if (!recursive) break;
    
    if (!unwrappable(prom)) {
      UNPROTECT(2);
      return prom;
    }
    next = unwrap_step(prom);

    if (next == R_UnboundValue) {
      break;
    } else {
      prom = next;
      REPROTECT(prom, prom_idx);
    }

    if (tortoise == prom) error("Circular promise chain!");
    tortoise = unwrap_step(tortoise);
    REPROTECT(tortoise, tortoise_idx);
    if (tortoise == prom) error("Circular promise chain!");
  }

  UNPROTECT(2);
  return prom;
}

/* Attempt to tease out a value from a promise without forcing.
   If unsuccessful return R_UnboundValue. */
SEXP peek_promise(SEXP prom) {
  LOG("got a %s", type2char(TYPEOF(prom)));
  
  prom = unwrap_promise(prom, TRUE);
  assert_type(prom, PROMSXP);
  while(TYPEOF(PREXPR(prom)) == PROMSXP) { //TODO: unwrap
    prom = PREXPR(prom);
  }
  LOG("unwrapped to a %s", type2char(TYPEOF(prom)));
  if (is_forced_promise(prom)) {
    LOG("it's forced - a %s", type2char(TYPEOF(PRVALUE(prom))));
    return PRVALUE(prom);
  }
  SEXP name = PREXPR(prom);
  SEXP env = PRENV(prom);
  switch(TYPEOF(name)) {
  case SYMSXP: {
    LOG("it's a %s, `%s`, quoted in environment %p",
        type2char(TYPEOF(name)), CHAR(PRINTNAME(name)), (void *) PRENV(prom));
    SEXP binding;
    binding = x_findVar(name, env);
    // we've already unwrapped so this shouldn't be another promise
    LOG("...bound to a  %s", type2char(TYPEOF(binding)));
    assert(TYPEOF(binding) != PROMSXP);
    return binding; // possibly R_UnboundValue
  }
  case LANGSXP:
    LOG("it's a call");
    return R_UnboundValue;
  default:
    LOG("it's a value");
    // this must be a value
    return name;
  }
}

SEXP _unwrap_quotation(SEXP q, SEXP recursive) {
  SEXP pr = PROTECT(_quotation_to_promsxp(q));
  pr = PROTECT(unwrap_promise(pr, asLogical(recursive)));
  q = PROTECT(promsxp_to_quotation(pr));
  UNPROTECT(3);
  return q;
}

/* selector for things arg_get finds */
typedef enum GET_ENUM {
  EXPR,
  ENV,
  VALUE,
  PROMISE,
  IS_LITERAL, /* not a "test" because it follows logic of inspecting expr/value */
  IS_MISSING
} GET_ENUM;

const char* get_enum_string(GET_ENUM type) {
  switch(type) {
  case EXPR: return "expression";
  case ENV: return "environment";
  case VALUE: return "value";
  case PROMISE: return "promise";
  case IS_LITERAL: return "is literal";
  case IS_MISSING: return "is missing";
  default: return "???";
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
  default: return "???";
  }
}

SEXP arg_get(SEXP, SEXP, GET_ENUM, int, int);

SEXP arg_get_from_quotation(SEXP quot, GET_ENUM request, int warn, SEXP value) {
  LOG("unwrapping a quotation\n");
  switch(request) {
  default:
  case EXPR:
    return _expr_quotation(quot);
  case ENV:
    return _env_quotation(quot);
  case VALUE:
    if (value != R_UnboundValue) return value;
    else return _value_quotation(quot);
  case PROMISE:
    if (value != R_UnboundValue) {
      LOG("there is a new value to include\n");
      if (is_forced_quotation(quot)) {
        return new_forced_promise(_expr_quotation(quot),
                                  value);
      } else {
        return new_weird_promise(_expr_quotation(quot),
                                 _env_quotation(quot),
                                 value);
      }
    } else {
      LOG("quotation to promsxp\n");
      return _quotation_to_promsxp(quot);
    }
  case IS_LITERAL:
    {
      SEXP expr = _expr_quotation(quot);
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
    }
  case IS_MISSING:
    if (_expr_quotation(quot) == R_MissingArg) {
      return ScalarLogical(TRUE);
    } else {
      return ScalarLogical(FALSE);
    }
  }
}

SEXP arg_get_from_unforced_promise(SEXP prom, GET_ENUM request, int warn) {
  SEXP expr = PREXPR(prom);
  switch(request) {
  default:
  case EXPR:
    return PREXPR(prom);
  case ENV:
    return PRENV(prom);
  case VALUE:
    error("Can't get value from unforced promise");
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
    {
      // note we have already unwrapped, back in arg_get
      if (PREXPR(prom) == R_MissingArg) { // should be the missing!
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
  case ENV:
    if (TYPEOF(PRENV(prom)) == ENVSXP) {
      /* weird promise is forced (has a value) but still has an env,
         and the value also duplicated to EXPR.  this happens in primitive
         function method dispatch for some reason */
      return PRENV(prom);
    }
  case EXPR:
  case VALUE:
  case PROMISE:
  default:
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
    default:
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
    case VALUE:
      return PRVALUE(prom);
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
    case VALUE:
      return PRVALUE(prom);      
    case IS_LITERAL: 
      /* the missing is a kind of literal */
      return ScalarLogical(expr == R_MissingArg);
    case IS_MISSING:
      /* Forced, so no unwrapping can be done */
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
    case VALUE:
      return PRVALUE(prom);      
    case IS_LITERAL:
      return ScalarLogical(FALSE);
    case IS_MISSING:
      return ScalarLogical(PRVALUE(prom) == R_MissingArg);
    }
  default:
    /* the expression is a data object but not a source literal */
    switch(request) {
    default:
    case ENV:
      return R_EmptyEnv;
    case EXPR:
      return expr;
    case VALUE:
      return PRVALUE(prom);
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
    default:
    case EXPR: 
      if (LENGTH(value) > 1 || ATTRIB(value) != R_NilValue) {
        if(warn) warning("`%s` not a promise, bound to non-scalar %s instead.",
                         CHAR(PRINTNAME(sym)),
                         type2char(TYPEOF(value)));
      }
      return value;
    case VALUE:
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
      default:
      case EXPR:
        return R_MissingArg;
      case VALUE:
        return value;
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
    } /* else fall through */
  case LANGSXP:
    switch(request) {
    default:
    case EXPR:
      if(warn) warning("`%s` not a promise, contains a %s.",
                       CHAR(PRINTNAME(sym)),
                       type2char(TYPEOF(value)));
      /* we are now making up `quote(x)` as a plausible way of
         having got an expression `x`. However since we also return
         EmptyEnv this should produce an error if the user attempts
         to re-call. */
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
    case VALUE:
      return value;
    case IS_LITERAL:
      return ScalarLogical(FALSE);
    case IS_MISSING:
      return ScalarLogical(FALSE);
    }
  default:
    switch(request) {
    default:
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
  SEXP ans;
  if (name == R_DotsSymbol) {
    error("Unsupported use of ... in arg_* (use `arg_list( (...) )` or get_dots())");    
  }
  LOG("Getting %s of binding `%s` in env `%p`\n",
      get_enum_string(type), CHAR(PRINTNAME(name)), (void *) envir);
  SEXP binding = PROTECT(x_findVar(name, envir));
  if (TYPEOF(binding) == PROMSXP) {
    if (recursive) binding = unwrap_promise(binding, recursive);
    while (TYPEOF(PREXPR(binding)) == PROMSXP) {
      binding = PREXPR(binding);
    }
    if (is_quotation(PREXPR(binding))) {
      ans = arg_get_from_quotation(PREXPR(binding), type, warn, PRVALUE(binding));
    } else if (PRVALUE(binding) != R_UnboundValue) {
      ans = arg_get_from_forced_promise(name, binding, type, warn);
    } else {
      ans = arg_get_from_unforced_promise(binding, type, warn);
    }
  } else {
    ans = arg_get_from_nonpromise(name, binding, type, warn);
  }
  UNPROTECT(1);
  return ans;
}

SEXP arg_check_from_quotation(SEXP quot, SEXP value,
                              TEST_ENUM query, int warn) {
  switch(query) {
  default:
  case IS_PROMISE:
    return ScalarLogical(TRUE);
  case IS_FORCED:
    return ScalarLogical((value != R_UnboundValue) || is_forced_quotation(quot));
  }
}

SEXP arg_check(SEXP envir, SEXP name, TEST_ENUM query, int warn) {
  assert_type(envir, ENVSXP);
  assert_type(name, SYMSXP);
  if (name == R_DotsSymbol) {error("Inappropriate use of ... in arg_*");}
  SEXP binding = do_findBinding(name, envir);
  while (TYPEOF(binding) == PROMSXP && TYPEOF(PREXPR(binding)) == PROMSXP) {
    /* Rprintf("Got a wrapped promise\n"); */
    // TODO: unwrap quotations here
    binding = PREXPR(binding);
  }
  switch(TYPEOF(binding)) {
  case PROMSXP:
    if (is_quotation(PREXPR(binding))) {
      return arg_check_from_quotation(PREXPR(binding), PRVALUE(binding), query, warn);
    }
    switch (query) {
    default:
    case IS_PROMISE: 
      return ScalarLogical(TRUE);
    case IS_FORCED:
      return is_forced_promise(binding) ? ScalarLogical(TRUE) : ScalarLogical(FALSE);
    }
  case SYMSXP:
    switch(query) {
    default:
    case IS_PROMISE:
      return ScalarLogical(FALSE);
    case IS_FORCED:
      /* missings are unforced by definition (and because compiler
         optimizes missing promsxps into missings */
      return ScalarLogical(binding != R_MissingArg);

    }
  default: 
    switch(query) {
    default:
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

SEXP _arg_value(SEXP envir, SEXP name, SEXP warn, SEXP sigil) {
  SEXP x = arg_get(envir, name, VALUE, asLogical(warn), FALSE);
  if (x ==  R_UnboundValue) return(sigil); else return x;
}

SEXP _arg_dots(SEXP envirs, SEXP syms, SEXP tags, SEXP warn) {
  assert_type(envirs, VECSXP);
  assert(TYPEOF(syms) == VECSXP || TYPEOF(syms) == STRSXP);
  if (tags != R_NilValue) {
    assert_type(tags, STRSXP);
    if (LENGTH(tags) != LENGTH(syms)) {
      error("Inputs to arg_dots have different lengths");
    }
  }
  if (LENGTH(envirs) != LENGTH(syms)) {
    error("Inputs to arg_dots have different lengths");
  }

  int len = LENGTH(syms);
  if (len == 0) {
    return R_NilValue;
  }

  /* so at least one item */
  SEXP head = PROTECT(allocSExp(DOTSXP));
  SEXP tail = head;
  Rboolean filled_head = FALSE;
  
# define APPEND(item, tag) {                    \
    PROTECT(item);                              \
    PROTECT(tag);                               \
    if (filled_head) {                          \
      SETCDR(tail, allocSExp(DOTSXP));          \
      tail = CDR(tail);                         \
    } else filled_head = TRUE;                  \
    SETCAR(tail, item);                         \
    SET_TAG(tail, tag);                         \
    UNPROTECT(2);                               \
  }

  for (int i = 0; i < len; i++) {
    SEXP sym;
    SEXP env = VECTOR_ELT(envirs, i);
    if (TYPEOF(syms) == STRSXP) {
      sym = installChar(STRING_ELT(syms, i));
    } else {
      sym = VECTOR_ELT(syms, i);
    }
    /* support args(a, b, (...)) as a way to get `...` along with your args */
    if (TYPEOF(sym) == LANGSXP) {
      assert2(length(sym) == 2, "Expected variable name");
      sym = CAR(CDR(sym));
    }
    assert_type(sym, SYMSXP);
    if (sym == R_DotsSymbol) {
      SEXP dots = _get_dots(env, ScalarLogical(TRUE));
      for (SEXP j = dots; j != R_NilValue; j = CDR(j)) {
        APPEND(CAR(j), TAG(j));
      }
    } else {
      LOG("Getting %s from env %p", CHAR(PRINTNAME(sym)), (void *) env);
      SEXP promise =
        arg_get(env, sym, PROMISE, asLogical(warn), FALSE);
      LOG("got a %s", type2char(TYPEOF(promise)));

      if (tags == R_NilValue) {
        assert_type(sym, SYMSXP);
        APPEND(promise, sym);
      } else {
        SEXP ch = STRING_ELT(tags, i);
        if (ch != R_BlankString) {
          APPEND(promise, installChar(ch));
        } else {
          APPEND(promise, R_NilValue);
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
