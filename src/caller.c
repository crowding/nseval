#include "vadr.h"
#include <Rinternals.h>
#include <setjmp.h>
#include <R_ext/Boolean.h>

int nullish(SEXP dots) {        /* R_NilValue but also list() */
  return (TYPEOF(dots) == VECSXP && LENGTH(dots) == 0);
}

SEXP do_(SEXP);

SEXP _do(SEXP dots) {
  assert_type(dots, VECSXP);
  dots = PROTECT(_flist_to_dotsxp(dots));
  SEXP result = do_(dots);
  UNPROTECT(1);
  return result;
}

SEXP do_(SEXP dots) {
  SEXP fun = CAR(dots);
  SEXP args = CDR(dots);

  assert_type(fun, PROMSXP);
  int has_args;
  if (nullish(dots)) {
    LOG("Nullish");
    has_args = 0;
  } else {
    assert_type(dots, DOTSXP);
    has_args = 1;
  }

  //construct a pairlist to make the call
  SEXP call;
  {
    int arglen = 0;
    if (has_args) arglen = length(dots);
    LOG("arglen = %d\n", arglen);
    call = PROTECT(allocList(arglen));
    SET_TYPEOF(call, LANGSXP);
  }

  //construct the call head
  SEXP callenv = PRENV(fun);
  if (PRVALUE(fun) != R_UnboundValue) {
    // call is a forced promise.
    SETCAR(call, fun);
    callenv = R_EmptyEnv;
    LOG("call head forced (a %s)\n",
        type2char(TYPEOF(CAR(call))));
  } else {
    SETCAR(call, PREXPR(fun));
    LOG("call head unforced (a %s)\n",
        type2char(TYPEOF(CAR(call))));
  };
    
  /* construct the call args (all input promises) */
  SEXP copyTo = call;
  if (has_args) {
    copyTo = CDR(copyTo);
    for (SEXP copyFrom = args; 
         copyFrom != R_NilValue && copyTo != R_NilValue; 
         copyFrom = CDR(copyFrom), copyTo = CDR(copyTo)) {

      SET_TAG(copyTo, TAG(copyFrom));
      SEXP thing = CAR(copyFrom);
      if (thing == R_MissingArg) {
        SETCAR(copyTo, thing);
      } else {
        assert_type(thing, PROMSXP);
        if (PRVALUE(thing) != R_UnboundValue) {
          if (PREXPR(thing) == PRVALUE(thing)
              && !is_language(PREXPR(thing))) {
            SETCAR(copyTo, PRVALUE(thing));
            LOG("copied 1 forced argument literally (a %s)\n",
                type2char(TYPEOF(CAR(copyTo))));
          } else {
            SETCAR(copyTo, thing);
            LOG("copied 1 forced argument directly (a %s)\n",
                type2char(TYPEOF(CAR(copyTo))));
          }
        } else/* if (PRVALUE(thing) == R_UnboundValue) */ {
          if (callenv == PRENV(thing)) {
            /* strip the promise so that it can work with R primitives */
            SETCAR(copyTo, PREXPR(thing));
            LOG("copied 1 argument unwrapped (a %s)\n", type2char(TYPEOF(CAR(copyTo))));
          } else {
            SETCAR(copyTo, thing);
            LOG("copied 1 unforced argument directly (a %s)\n",
                type2char(TYPEOF(CAR(copyTo))));
          }
        }
      }
    }
  }

  // for the called code to see its caller, we wrap the call in
  // a promsxp that we make the call "from"
  SEXP from = PROTECT(new_promise(call, callenv));

  // now eval, in the given environment.
  LOG("Evaluating %s %p in %s %p\n",
      type2char(TYPEOF(call)), call,
      type2char(TYPEOF(callenv)), callenv);
  SEXP result = PROTECT(eval(from, callenv));
  LOG("got a %s\n", type2char(TYPEOF(result)));
  UNPROTECT(3);
  return result;
}
