#include "vadr.h"
#include "promises.h"

int _dots_length(SEXP dots);
SEXP emptypromise();

SEXP _get_dots(SEXP env, SEXP inherit) {
  assert_type(env, ENVSXP);
  SEXP vl;
  LOG("Getting dots from env %p", env);
  if (asLogical(inherit)) {
    vl = findVar(R_DotsSymbol, env);
  } else {
    vl = findVarInFrame3(env, R_DotsSymbol, TRUE);
  }
  if (vl == R_UnboundValue || vl == R_MissingArg) {
    LOG("... not found in env %p", env);
    return R_NilValue;
  } else {
    return vl;
  }
}

SEXP _set_dots(SEXP dots, SEXP env) {
  assert_type(env, ENVSXP);
  if (isNull(dots) || dots == R_MissingArg) {
    defineVar(R_DotsSymbol, R_MissingArg, env); /* is this kosher? */       
  } else {
    assert_type(dots, DOTSXP);
    defineVar(R_DotsSymbol, dots, env);
  }
  return R_NilValue;
}

SEXP _nseval_update(SEXP name, SEXP env, SEXP value, SEXP inherit) {
  assert_type(env, ENVSXP);
  assert_type(name, SYMSXP);
  assert_type(value, SYMSXP);
  return R_NilValue;
}

/* measure the length of a dots object. */
int _dots_length(SEXP dots) {
  SEXP s;
  int length;
  switch (TYPEOF(dots)) {
  case NILSXP:
    return 0;
    break;
  case VECSXP:
    if (LENGTH(dots) == 0) return 0;
    break;
  case LISTSXP:
  case DOTSXP:
    for (s = dots, length = 0; s != R_NilValue; s = CDR(s)) length++;
    return length;
  }
  error("Expected dotlist or pairlist, got %s", type2char(TYPEOF(dots)));
  return 0;
}

SEXP _dots_unpack(SEXP dots) {
  SEXP names, environments, expressions, values;

  SEXP dataFrame;
  SEXP colNames;

  //check inputs and measure length
  assert_type(dots, VECSXP);
  int length = LENGTH(dots);

  // unpack information for each item:
  // names, environemnts, expressions, values
  PROTECT(names = allocVector(STRSXP, length));
  PROTECT(environments = allocVector(VECSXP, length));
  PROTECT(expressions = allocVector(VECSXP, length));
  PROTECT(values = allocVector(VECSXP, length));

  // hmm could just re-use the names eh?
  SEXP input_names = getAttrib(dots, R_NamesSymbol);
  
  for (int i = 0; i < length; i++) {
    SEXP item = PROTECT(_quotation_to_promsxp(VECTOR_ELT(dots, i)));
    SEXP tag = (input_names == R_NilValue) ? R_BlankString : STRING_ELT(input_names, i); 

    if ((TYPEOF(PRENV(item)) != ENVSXP) && (PRENV(item) != R_NilValue))
      error("Expected ENVSXP or NULL in environment slot of DOTSXP, got %s",
            type2char(TYPEOF(item)));

    SET_VECTOR_ELT(environments, i, PRENV(item));
    SET_VECTOR_ELT(expressions, i, PREXPR(item));
    SET_STRING_ELT(names, i, tag);

    if (PRVALUE(item) != R_UnboundValue) {
      SET_VECTOR_ELT(values, i, PRVALUE(item));
    } else {
      SET_VECTOR_ELT(values, i, R_NilValue);
    }
    UNPROTECT(1);
  }
  PROTECT(dataFrame = allocVector(VECSXP, 4));
  SET_VECTOR_ELT(dataFrame, 0, names);
  SET_VECTOR_ELT(dataFrame, 1, environments);
  SET_VECTOR_ELT(dataFrame, 2, expressions);
  SET_VECTOR_ELT(dataFrame, 3, values);

  PROTECT(colNames = allocVector(STRSXP, 4));
  SET_STRING_ELT(colNames, 0, mkChar("name"));
  SET_STRING_ELT(colNames, 1, mkChar("envir"));
  SET_STRING_ELT(colNames, 2, mkChar("expr"));
  SET_STRING_ELT(colNames, 3, mkChar("value"));

  setAttrib(dataFrame, R_NamesSymbol, colNames);
  setAttrib(dataFrame, R_RowNamesSymbol, names);
  setAttrib(dataFrame, R_ClassSymbol, mkString("data.frame"));

  UNPROTECT(6);
  return(dataFrame);
}

SEXP _dots_names(SEXP dots) {
  SEXP names, s;
  int i, length;

  length = _dots_length(dots);

  int made = 0;
  names = R_NilValue;
  PROTECT(names = allocVector(STRSXP, length));

  for (s = dots, i = 0; i < length; s = CDR(s), i++) {
    if (isNull(TAG(s))) {
      SET_STRING_ELT(names, i, R_BlankString);
    } else {
      made = 1;
      SET_STRING_ELT(names, i, PRINTNAME(TAG(s)));
    }
  }
  UNPROTECT(1);
  
  return(made ? names : R_NilValue);
}

SEXP _dots_exprs(SEXP dots) {
  SEXP names, s, expressions;
  int i, length;
  
  length = _dots_length(dots);
  names = PROTECT(_dots_names(dots));

  PROTECT(expressions = allocVector(VECSXP, length));

  for (s = dots, i = 0; i < length; s = CDR(s), i++) {
    SEXP item = CAR(s);
    // if we have an unevaluated promise whose code is another promise, descend
    while ((PRENV(item) != R_NilValue) && (TYPEOF(PRCODE(item)) == PROMSXP)) {
      item = PRCODE(item);
    }
    SET_VECTOR_ELT(expressions, i, PREXPR(item));    
  }

  if (names != R_NilValue)
    setAttrib(expressions, R_NamesSymbol, names);

  UNPROTECT(2);
  return(expressions);
}

SEXP _dots_envs(SEXP dots) {
  SEXP names, s, envs;
  int i, length;

  length = _dots_length(dots);
  names = PROTECT(_dots_names(dots));
  PROTECT(envs = allocVector(VECSXP, length));

  for (s = dots, i = 0; i < length; s = CDR(s), i++) {
    SEXP item = CAR(s);
    // if we have an unevaluated promise whose code is another promise, descend
    while ((PRENV(item) != R_NilValue) && (TYPEOF(PRCODE(item)) == PROMSXP)) {
      item = PRCODE(item);
    }
    SET_VECTOR_ELT(envs, i, PRENV(item));    
  }

  if (names != R_NilValue)
    setAttrib(envs, R_NamesSymbol, names);

  UNPROTECT(2);
  
  return(envs);
}

SEXP _flist_to_dotsxp(SEXP flist) {
  assert_type(flist, VECSXP);
  int len = LENGTH(flist);
  int i;
  SEXP output, names;
  names = getAttrib(flist, R_NamesSymbol);
  if (len == 0) {
    return R_NilValue;
  } else {
    output = PROTECT(allocList(len));
    SEXP output_iter = output;
    for (i = 0; i < len; i++, output_iter=CDR(output_iter)) {
      SET_TYPEOF(output_iter, DOTSXP);
      if ((names != R_NilValue) && (STRING_ELT(names, i) != R_BlankString)) {
        SET_TAG(output_iter, install(CHAR(STRING_ELT(names, i))));
      } else {
        SET_TAG(output_iter, R_NilValue);
      }
      SEXP clos = VECTOR_ELT(flist, i);
      SETCAR(output_iter, _quotation_to_promsxp(clos));
    }
    UNPROTECT(1);
    return output;
  }
}

SEXP map_pairlist_to_list(SEXP in, SEXP (*f)(SEXP)) {
  int i;
  SEXP output;
  int len = length(in);
  int protections = 0;
  SEXP names = R_NilValue;

  if (in == R_NilValue) {
    PROTECT(output = allocVector(VECSXP, 0)); protections++;
    
  } else if ((TYPEOF(in) != DOTSXP) && (TYPEOF(in) != LISTSXP)) {
    error("Expected dotlist or pairlist, got %s", type2char(TYPEOF(in)));
    
  } else {
    PROTECT(output = allocVector(VECSXP, len)); protections++;
    
    for (i = 0; i < len; in=CDR(in), i++) {
      
      SEXP result = PROTECT((*f)(CAR(in)));
      SET_VECTOR_ELT(output, i, result);
      UNPROTECT(1);

      if (!isNull(TAG(in))) {
        if (names == R_NilValue) {
          PROTECT(names = allocVector(STRSXP, len)); protections++;
        }
        SET_STRING_ELT(names, i, PRINTNAME(TAG(in)));    
      } else {
        if (names != R_NilValue) {
          SET_STRING_ELT(names, i, R_BlankString);
        }
      }
    }

    if (names != R_NilValue) {
      setAttrib(output, R_NamesSymbol, names);
    }
  }

  UNPROTECT(protections);
  return output;
}

SEXP promisish_to_closxp(SEXP x) {
  SEXP out;
  int protections = 0;
  if (TYPEOF(x) == PROMSXP) {
    out = PROTECT(promsxp_to_quotation(x));
    protections++;
  } else if (x == R_MissingArg) {
    out = PROTECT(empty_closure());
    protections++;
  } else {
    // this doesn't seem to happen, aside from missings. Not to say it
    // can't.
    warning("nonpromise (a %s, %p) found in ... list",
            type2char(TYPEOF(x)), x);
    if (is_language(x)) {
      SEXP quote = PROTECT(Rf_lang2(install("quote"), x));
      protections++;
      /* we are now making up `quote(x)` as a plausible way of
         having got the symbol `x`. However since we also return
         EmptyEnv this will produce an error if the user attempts
         to re-call. */
      x = PROTECT(new_forced_promise(quote, x));
      protections++;
    } else {
      x = PROTECT(new_forced_promise(x, x));
      protections++;
    }
    out = PROTECT(promsxp_to_quotation(x));
    protections++;
  }
  setAttrib(out, R_ClassSymbol, mkString("quotation"));
  UNPROTECT(protections);
  return out;
}

/* Convert a DOTSXP into a list of closures, perhaps use raw objects for closures... */
SEXP _dotsxp_to_flist(SEXP d) {
  if (d == R_MissingArg) { // when "..." is missing as opposed to
                           // unbound (does this happen)?
    d = R_NilValue;
  }
  
  SEXP out = PROTECT(map_pairlist_to_list(d, &promisish_to_closxp));
  setAttrib(out, R_ClassSymbol, mkString("dots"));
  
  UNPROTECT(1);
  return out;
}

int is_list_type(SEXPTYPE t) {
  switch (t) {
  case LISTSXP:
  case DOTSXP:
  case LANGSXP:
    return 1;
  default:
    return 0;
  }
}

int is_list_like(SEXP in) {
  return is_list_type(TYPEOF(in));
}

/* given pointers to head and tail SEXPs, appends an new object.  Will
   PROTECT when head of list is created, So you need to check an
   unprotect head when done with list. */
void append_item(SEXP *head, SEXP *tail, SEXPTYPE type, SEXP tag, SEXP obj) {
  if (tag != R_NilValue) assert_type(tag, SYMSXP);
  if (*tail == R_NilValue) {
    if (is_list_type(type)) {
      *head = PROTECT(allocSExp(type));
      *tail = *head;
      SET_TAG(*tail, tag);
      SETCAR(*tail, obj);
    } else {
      error("Type should be pairlist-like (got %s)", type2char(type));
    }
  } else {
    if (is_list_like(*tail)) {
      SETCDR (*tail, allocSExp(type));
      *tail = CDR(*tail);
      SET_TAG(*tail, tag);
      SETCAR(*tail, obj);
    } else {
      error("Tail was not pairlist-like (got %s)", type2char(TYPEOF(*tail)));
    }
  }
}


/* Extract named variables from an environment into a dotslist */
SEXP _env_to_dots(SEXP envir, SEXP names, SEXP missing, SEXP expand) {
  /* Note that I'm still working internally in DOTSXPs.  However, raw
     dotsxps and promsxps should not be returned from this package into
     userspace. */
  assert_type(envir, ENVSXP);
  assert_type(names, STRSXP);
  int use_missing = asLogical(missing);
  int expand_dots = asLogical(expand);
  int length = LENGTH(names);

  /* e.g., growList(&head, &tail, DOTSXP, tag, obj) */
  SEXP head = R_NilValue;
  SEXP tail = R_NilValue;
  
  for (int i = 0; i < length; i++) {
    SEXP sym = installChar(STRING_ELT(names, i));
    SEXP found = findVar(sym, envir);
    if (found == R_UnboundValue) {
      error("Variable `%s` was not found.",
            CHAR(PRINTNAME(sym)));
    }
    
    /* unwrap promise chain */
    while (TYPEOF(found) == PROMSXP && TYPEOF(PRCODE(found)) == PROMSXP) {
      found = PRCODE(found);
    }
    
    if (!use_missing
        && (found == R_MissingArg
            || (TYPEOF(found) == PROMSXP
                && PRCODE(found) == R_MissingArg))) {
      continue;
    }

    if (sym == R_DotsSymbol) {
      if (expand_dots && found != R_MissingArg) {
        assert_type(found, DOTSXP);
        while (is_list_like(found)) {
          append_item(&head, &tail, DOTSXP, TAG(found), CAR(found));
          found = CDR(found);
        }
      }
    } else {
      append_item(&head, &tail, DOTSXP, sym, make_into_promsxp(found));
    }   
  }
  SEXP out = PROTECT(_dotsxp_to_flist(head));
  setAttrib(out, R_ClassSymbol, ScalarString(mkChar("dots")));
  UNPROTECT(1);
  if (head != R_NilValue) {
    /* unprotect owed to append_item. */
    UNPROTECT(1);
  }
  return out;
}


/* Add the entries in a dotsxp to the given environment;
 * if untagged, append to ... */
SEXP _dots_to_env(SEXP dots, SEXP envir, SEXP newdots) {
  if (dots != R_NilValue && dots != R_MissingArg) {
    assert_type(dots, DOTSXP);
    assert_type(envir, ENVSXP);
    for(SEXP iter = dots; iter != R_NilValue; iter = CDR(iter)) {
      if (TAG(iter) == R_NilValue) error("Attempt to assign variable with no name");
      if (TAG(iter) == R_MissingArg) error("Illegal variable name ``");
      if (TAG(iter) == R_DotsSymbol) error("Illegal variable name `...`");
      defineVar(TAG(iter), CAR(iter), envir);
    }
  }
  if (newdots != R_NilValue) {
    assert_type(newdots, DOTSXP);
    defineVar(R_DotsSymbol, newdots, envir);
  }
  return envir;
}
