#include "caller.h"

SEXP _make_call(SEXP fdots, SEXP envir, SEXP dots) {
  assert_type3(fdots, DOTSXP, "f_dots must be DOTSXP");
  assert_type3(envir, ENVSXP, "envir is not an environment");
  int has_args;
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
    for (SEXP x = fdots; x != R_NilValue; x = CDR(x)) arglen++;
    if (has_args)
      for (SEXP x = dots; x != R_NilValue; x = CDR(x)) arglen++;
    
    call = PROTECT(allocList(arglen));
    SET_TYPEOF(call, LANGSXP);
  }
  
  // for the called code to see its caller, we wrap the call in
  // a promsxp that we make the call "from"
  // This may mean that...
  // TODO: verify that this is really necessary
  {
    SEXP copyTo = call;
    for (SEXP copyFrom = fdots; 
         copyFrom != R_NilValue && copyTo != R_NilValue; 
         copyFrom = CDR(copyFrom), copyTo = CDR(copyTo)) {
      SET_TAG(copyTo, TAG(copyFrom));
      SETCAR(copyTo, CAR(copyFrom));
    }
    
    if (has_args) {
      for (SEXP copyFrom = dots; 
           copyFrom != R_NilValue && copyTo != R_NilValue; 
           copyFrom = CDR(copyFrom), copyTo = CDR(copyTo)) {
        SET_TAG(copyTo, TAG(copyFrom));
        SETCAR(copyTo, CAR(copyFrom));
      }
    }
  }
  
  SEXP from = PROTECT(allocSExp(PROMSXP));
  SET_PRENV(from, envir);
  SET_PRCODE(from, call);
  SET_PRVALUE(from, R_UnboundValue);
  
  //this may be a new test case for .caller: eval called from inside .call
  SEXP out = PROTECT(eval(from, envir));
  UNPROTECT(3);
  return out;
}
