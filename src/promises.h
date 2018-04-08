#ifndef PROMISES_H
#define PROMISES_H

SEXP promsxp_to_quotation(SEXP);
SEXP empty_closure();
SEXP _quotation_to_promsxp(SEXP);
SEXP promsxp_to_quotation(SEXP);
SEXP make_into_promsxp(SEXP);
int _is_forced_quotation(SEXP);

#endif
