#ifndef PROMISES_H
#define PROMISES_H

SEXP promsxp_to_closxp(SEXP prom);
SEXP empty_closure();
SEXP closxp_to_promsxp(SEXP clos);
SEXP make_into_promise(SEXP in);
int _is_forced_f(SEXP clos);

#endif
