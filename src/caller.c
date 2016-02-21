#include "caller.h"

SEXP _caller(SEXP env) {
  assert_type3(env, ENVSXP, "Agument to caller must be an environment");

  RCNTXT *c = (RCNTXT *) R_GlobalContext;

  return(c->cloenv);
}
