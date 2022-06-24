#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _arg(SEXP, SEXP, SEXP);
extern SEXP _arg_dots(SEXP, SEXP, SEXP, SEXP);
extern SEXP _arg_env(SEXP, SEXP, SEXP);
extern SEXP _arg_expr(SEXP, SEXP, SEXP);
extern SEXP _arg_value(SEXP, SEXP, SEXP, SEXP);
extern SEXP _construct_do_call(SEXP);
extern SEXP _dots_envs(SEXP);
extern SEXP _dots_exprs(SEXP);
extern SEXP _dots_to_env(SEXP, SEXP, SEXP);
extern SEXP _dots_unpack(SEXP);
extern SEXP _dotsxp_to_quolist(SEXP);
extern SEXP _env_to_dots(SEXP, SEXP, SEXP, SEXP);
extern SEXP _env_quotation(SEXP);
extern SEXP _expr_quotation(SEXP);
extern SEXP _flist_to_dotsxp(SEXP);
extern SEXP _is_forced_quotation(SEXP);
extern SEXP _is_plausible_quotation(SEXP);
extern SEXP _get_dots(SEXP, SEXP);
extern SEXP _is_forced(SEXP, SEXP, SEXP);
extern SEXP _is_literal(SEXP, SEXP, SEXP);
extern SEXP _is_missing(SEXP, SEXP, SEXP, SEXP);
extern SEXP _is_promise(SEXP, SEXP, SEXP);
extern SEXP _locate(SEXP, SEXP, SEXP);
extern SEXP _quotation(SEXP, SEXP, SEXP, SEXP);
extern SEXP _quotation_old(SEXP, SEXP, SEXP);
extern SEXP _quotation_literal(SEXP);
extern SEXP _quotation_to_promsxp(SEXP);
extern SEXP _remove(SEXP, SEXP);
extern SEXP _set_dots(SEXP, SEXP);
extern SEXP _unwrap_quotation(SEXP, SEXP);
extern SEXP _value_quotation(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_arg",                  (DL_FUNC) &_arg,                  3},
  {"_arg_dots",             (DL_FUNC) &_arg_dots,             4},
  {"_arg_env",              (DL_FUNC) &_arg_env,              3},
  {"_arg_expr",             (DL_FUNC) &_arg_expr,             3},
  {"_arg_value",            (DL_FUNC) &_arg_value,            4},
  {"_construct_do_call",    (DL_FUNC) &_construct_do_call,    1},
  {"_dots_envs",            (DL_FUNC) &_dots_envs,            1},
  {"_dots_exprs",           (DL_FUNC) &_dots_exprs,           1},
  {"_dots_to_env",          (DL_FUNC) &_dots_to_env,          3},
  {"_dots_unpack",          (DL_FUNC) &_dots_unpack,          1},
  {"_dotsxp_to_quolist",    (DL_FUNC) &_dotsxp_to_quolist,    1},
  {"_env_to_dots",          (DL_FUNC) &_env_to_dots,          4},
  {"_expr_quotation",       (DL_FUNC) &_expr_quotation,       1},
  {"_env_quotation",        (DL_FUNC) &_env_quotation,        1},
  {"_flist_to_dotsxp",      (DL_FUNC) &_flist_to_dotsxp,      1},
  {"_is_forced_quotation",  (DL_FUNC) &_is_forced_quotation,  1},
  {"_is_plausible_quotation",  (DL_FUNC) &_is_plausible_quotation,  1},
  {"_get_dots",             (DL_FUNC) &_get_dots,             2},
  {"_is_forced",            (DL_FUNC) &_is_forced,            3},
  {"_is_literal",           (DL_FUNC) &_is_literal,           3},
  {"_is_missing",           (DL_FUNC) &_is_missing,           4},
  {"_is_promise",           (DL_FUNC) &_is_promise,           3},
  {"_locate",               (DL_FUNC) &_locate,               3},
  {"_quotation",            (DL_FUNC) &_quotation,            4},
  {"_quotation_old",        (DL_FUNC) &_quotation_old,        3},
  {"_quotation_literal",    (DL_FUNC) &_quotation_literal,    1},
  {"_quotation_to_promsxp", (DL_FUNC) &_quotation_to_promsxp, 1},
  {"_remove",               (DL_FUNC) &_remove, 2},
  {"_set_dots",             (DL_FUNC) &_set_dots,             2},
  {"_unwrap_quotation",     (DL_FUNC) &_unwrap_quotation,     2},
  {"_value_quotation",      (DL_FUNC) &_value_quotation,      1},
  {NULL, NULL, 0}
};

void R_init_nseval(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
