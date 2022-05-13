#include "vadr.h"
#include <Rinternals.h>
#include <setjmp.h>
#include <R_ext/Boolean.h>

int nullish(SEXP dots) {        /* R_NilValue but also list() */
  return (TYPEOF(dots) == VECSXP && LENGTH(dots) == 0);
}

SEXP _construct_do_call(SEXP dots) {
  dots = PROTECT(_flist_to_dotsxp(dots));

  //return a list with 3 items: call, env, dotsxp
  SEXP out = PROTECT(allocVector(VECSXP, 3));
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
  LOG("allocating call");
  SEXP call;
  {
    int arglen = 0;
    if (has_args) arglen = length(dots);
    LOG("arglen = %d\n", arglen);
    SET_VECTOR_ELT(out, 0, call = allocList(arglen));
    SET_TYPEOF(call, LANGSXP);
  }

  //construct the call head
  SEXP callenv = PRENV(fun);
  if (PRVALUE(fun) != R_UnboundValue) {
    // call is a forced promise. Dutifully we call "from" nowhere
    SETCAR(call, fun);
    callenv = R_EmptyEnv;
    LOG("call head forced (a %s)\n",
        type2char(TYPEOF(CAR(call))));
  } else {
    SETCAR(call, PREXPR(fun));
    LOG("call head unforced (a %s)\n",
        type2char(TYPEOF(CAR(call))));
  };
  SET_VECTOR_ELT(out, 1, callenv);

  // determine whether we can we assign a temp `...`
  Rboolean using_dots = !(R_EnvironmentIsLocked(callenv) || callenv == R_EmptyEnv);

  /* construct the call args (all input promises) */
  SEXP copyTo = call;
  if (has_args) {
    copyTo = CDR(copyTo);
    SEXP copyFrom = args;
    for (; 
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
            LOG("copied 1 forced argument literally (a %s)",
                type2char(TYPEOF(CAR(copyTo))));
          } else {
            if (using_dots) {
              break;
            } else {
              // if we can't use ... then we have to put a promise
              // directly in the call, (which may lead to stack traces
              // containing unprintable objects.)
              SETCAR(copyTo, thing);
              LOG("copied 1 forced argument directly (a %s)\n",
                  type2char(TYPEOF(CAR(copyTo))));
            }
          }
        } else /* if (PRVALUE(thing) == R_UnboundValue) */ {
          if (callenv == PRENV(thing)) {
            /* strip the promise so that it can work with R primitives */
            SETCAR(copyTo, PREXPR(thing));
            LOG("copied 1 argument unwrapped (a %s)\n", type2char(TYPEOF(CAR(copyTo))));
          } else {
            if (using_dots) {
              break;
            } else {
              // directly putting a promise in the call 
              SETCAR(copyTo, thing);
              LOG("copied 1 unforced argument directly (a %s)\n",
                  type2char(TYPEOF(CAR(copyTo))));
            }
          }
        }
      }
    }
    if (using_dots && copyFrom != R_NilValue && copyTo != R_NilValue) {
      LOG("putting rest of arguments into `...`");
      SETCAR(copyTo, R_DotsSymbol);
      SETCDR(copyTo, R_NilValue);
      SET_VECTOR_ELT(out, 2, copyFrom);
    } else {
      SET_VECTOR_ELT(out, 2, R_NilValue);
    }
  }
  // for the call to appear nicely in sys.calls, we wrap the call in
  // a promsxp that we make the call "from"
  SET_VECTOR_ELT(out, 0, new_promise(call, callenv));
  UNPROTECT(2);
  return out;
}
