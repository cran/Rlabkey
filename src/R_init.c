#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "RJSON_parser.h"

/* .Call calls */
extern SEXP Rlabkey_listToMatrix(SEXP, SEXP);

R_CallMethodDef callMethods[] = {
	{ ".JSON_to_R", (DL_FUNC) &JSON_to_R, 1},
	{ ".parser_new", (DL_FUNC) &parser_new, 0},
	{ ".parser_add", (DL_FUNC) &parser_new, 3},
	{ ".parser_finalize", (DL_FUNC) &parser_new, 1},
	{ ".parser_delete", (DL_FUNC) &parser_new, 1},
	{ "Rlabkey_listToMatrix", (DL_FUNC) &Rlabkey_listToMatrix, 2},
	{NULL, NULL, 0}
};

void
R_init_Rlabkey(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
}
