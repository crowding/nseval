#ifndef PROMISES_H
#define PROMISES_H

SEXP forced_value_promise(SEXP in);
SEXP promsxp_to_quotation(SEXP);
SEXP empty_closure(void);
SEXP _quotation_to_promsxp(SEXP);
SEXP promsxp_to_quotation(SEXP);
SEXP make_into_promsxp(SEXP);
SEXP _expr_quotation(SEXP);
SEXP _env_quotation(SEXP);
SEXP _value_quotation(SEXP);
SEXP _quotation(SEXP, SEXP, SEXP, SEXP);
int is_quotation(SEXP);
int is_plausible_quotation(SEXP);
int is_forced_quotation(SEXP);
SEXP _is_forced_quotation(SEXP);
Rboolean is_weird_quotation(SEXP quot);

#endif
