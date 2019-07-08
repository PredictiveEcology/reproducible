#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP _reproducible_readLinesRcppInternal(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_reproducible_readLinesRcppInternal", (DL_FUNC) &_reproducible_readLinesRcppInternal, 1},
  {NULL, NULL, 0}
};

void R_init_reproducible(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
