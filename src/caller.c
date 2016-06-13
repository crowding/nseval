#include "caller.h"

SEXP _make_call(SEXP f, SEXP envir, SEXP dots) {
  assert_type3(envir, ENVSXP, "envir is not an environment");
  int has_args;
  
  //possibly empty dots object...
  if (TYPEOF(dots) == VECSXP && LENGTH(dots) == 0) {
    has_args = 0;
  } else {
    assert_type3(dots, DOTSXP, "dots");
    has_args = 1;
  }
  

  //construct a pairlist to make the call
  SEXP call;
  {
    int arglen = 0;
    if (has_args)
      for (SEXP x = dots; x != R_NilValue; x = CDR(x)) arglen++;
    
    call = PROTECT(allocList(arglen + 1));
    SET_TYPEOF(call, LANGSXP);
  }
  
  //construct the call args (all input promises)
  {
    SEXP copyTo = call;
    SETCAR(copyTo, f);
    if (has_args) {
      copyTo = CDR(copyTo);
      for (SEXP copyFrom = dots; 
           copyFrom != R_NilValue && copyTo != R_NilValue; 
           copyFrom = CDR(copyFrom), copyTo = CDR(copyTo)) {
        SET_TAG(copyTo, TAG(copyFrom));
        SETCAR(copyTo, CAR(copyFrom));
      }
    }
  }
  
  // for the called code to see its caller, we wrap the call in
  // a promsxp that we make the call "from"
  // TODO: verify that this is really necessary
  SEXP from = PROTECT(allocSExp(PROMSXP));
  SET_PRENV(from, envir);
  SET_PRCODE(from, call);
  SET_PRVALUE(from, R_UnboundValue);
  
  UNPROTECT(2);
  return call;
}
