#ifndef PROMISES_H
#define PROMISES_H

SEXP forced_value_promise(SEXP in);
SEXP promsxp_to_quotation(SEXP);
SEXP empty_closure();
SEXP _quotation_to_promsxp(SEXP);
SEXP promsxp_to_quotation(SEXP);
SEXP make_into_promsxp(SEXP);
SEXP _is_forced_quotation(SEXP);
SEXP _quotation(SEXP, SEXP, SEXP, SEXP);

#endif
