#ifndef _VADR_H
#define _VADR_H

#include <R.h>
#include <Rinternals.h>

#undef DEBUG 
// #define DEBUG

#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#define MIN3(x, y, z) (MIN(x,(MIN(y,z))))
#define MAX3(x, y, z) (MAX(x,(MAX(y,z))))

#define assert_type(X, T) {                                             \
    if (TYPEOF(X) != T)                                                 \
      error("%s: expected %s, got %s, at@%s:%d",                        \
            __func__,                                                   \
            type2char(T),                                               \
            type2char(TYPEOF(X)),                                       \
            __FILE__,                                                   \
            __LINE__);                                                  \
  }

#define assert(COND) {                            \
    assert2(COND, "Assertion failed: " #COND);    \
  }

#define assert2(COND, MSG) {                                    \
  if (!(COND)) {                                                \
  error("%s: %s @%s:%d\n",                                    \
        __func__, MSG, __FILE__, __LINE__);                   \
  }                                                           \
}

#define assertn(COND, FMT, ...) {                                \
    if (!(COND)) {                                               \
      error("%s: " FMT " @%s:%d\n",                            \
            __func__, ##__VA_ARGS__, __FILE__, __LINE__);        \
    }                                                            \
  }



#ifdef DEBUG
#define LOG(FMT, ...) Rprintf("%s: "  FMT " @%s:%d\n",                  \
                              __func__, ##__VA_ARGS__, __FILE__, __LINE__)
#else
#define LOG(...) NULL
#endif

void assert_type3(SEXP, SEXPTYPE, const char *);
int recycle_length(int i, int j);

SEXP emptypromise(void);
SEXP new_promise(SEXP expr, SEXP env);
SEXP new_forced_promise(SEXP expr, SEXP value);
SEXP new_weird_promise(SEXP expr, SEXP value, SEXP env);
SEXP x_findVar(SEXP sym, SEXP envir);

SEXP _flist_to_dotsxp(SEXP flist);
int is_language(SEXP x);
int is_forced(SEXP x);
SEXP peek_promise(SEXP prom);

#endif
