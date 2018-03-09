#ifndef PROMISES_H
#define PROMISES_H

SEXP promsxp_to_quotation(SEXP prom);
SEXP empty_closure();
SEXP quotation_to_promsxp(SEXP clos);
SEXP make_into_promsxp(SEXP in);
int _is_forced_quotation(SEXP clos);

#endif
