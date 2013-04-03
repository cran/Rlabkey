/*
 * Copyright (c) 2010 Fred Hutchinson Cancer Research Center
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <Rdefines.h>
#include <assert.h>
#include "JSON_parser.h"
#include "RJSON_parser.h"

/*
 *  A buffer holding json-parsed values expands as a linked list.
 */

const size_t pool_size = 1024;

/* _type_pool */

typedef struct _type_pool_struct {
	size_t i, n;
	JSON_type *type;
	struct _type_pool_struct *prev;
} _type_pool;

_type_pool *
_type_pool_new(_type_pool *prev)
{
	_type_pool *pool = Calloc(1, _type_pool);
	pool->type = Calloc(pool_size, JSON_type);
	pool->n = pool_size;
	pool->i = 0;
	pool->prev = prev;
	return pool;
}

_type_pool *
_type_pool_delete(_type_pool *pool)
{
	if (NULL == pool)
		return NULL;
	_type_pool *prev = pool->prev;
	Free(pool->type);
	Free(pool);
	return prev;
}

_type_pool *
_type_pool_push(_type_pool *pool, JSON_type type)
{
	if (pool->i == pool->n)
		pool = _type_pool_new(pool);
	pool->type[pool->i++]= type;
	return pool;
}

JSON_type
_type_pool_get(_type_pool *pool)
{
	if (0 == pool->i)
		Rf_error("'RJSON_parser internal: 'get' on _type_pool");
	return pool->type[pool->i - 1];
}

JSON_type
_type_pool_pop(_type_pool **pool)
{
	JSON_type type = _type_pool_get(*pool);
	if (1 != (*pool)->i)
		(*pool)->i -= 1;
	else
		*pool = _type_pool_delete(*pool);
	return type;
}

/* _value_pool */

typedef struct _value_pool_struct {
	size_t i, n;
	JSON_value *value;
	struct _value_pool_struct *prev;
} _value_pool;

_value_pool *
_value_pool_new(_value_pool *prev)
{
	_value_pool *pool = Calloc(1, _value_pool);
	pool->value = Calloc(pool_size, JSON_value);
	pool->n = pool_size;
	pool->i = 0;
	pool->prev = prev;
	return pool;
}

_value_pool *
_value_pool_delete(_value_pool *pool)
{
	if (NULL == pool)
		return NULL;
	_value_pool *prev = pool->prev;
	Free(pool->value);
	Free(pool);
	return prev;
}

_value_pool *
_value_pool_push(_value_pool *pool, const JSON_value *value)
{
	if (pool->i == pool->n)
		pool = _value_pool_new(pool);
	pool->value[pool->i++] = *value;
	return pool;
}

JSON_value
_value_pool_get(_value_pool *pool)
{
	if (0 == pool->i)
		Rf_error("RJSON_parser internal: 'get' on empty _value_pool");
	return pool->value[pool->i - 1];
}

JSON_value
_value_pool_pop(_value_pool **pool)
{
	JSON_value value = _value_pool_get(*pool);
	if (1 != (*pool) -> i)
		(*pool)->i -= 1;
	else
		*pool = _value_pool_delete(*pool);
	return value;
}

/* _r_pool: pairlist, memory managed mostly by R */

typedef SEXP _r_pool_value;
typedef SEXP _r_pool;

_r_pool
_r_pool_new()
{
	return CONS(R_NilValue, R_NilValue); /* this will be protected */
}

_r_pool
_r_pool_push(_r_pool pool, _r_pool_value rvalue)
{
	SEXP node = CONS(rvalue, CDR(pool));
	SETCDR(pool, node);
	return pool;
}

_r_pool_value
_r_pool_get(_r_pool pool)
{
	return CADR(pool);
}

_r_pool_value
_r_pool_pop(_r_pool pool)
{
	_r_pool_value rvalue = CADR(pool);
	SEXP node = CDDR(pool);
	SETCDR(pool, node);
	return rvalue;
}

/* RJSON_callback_ctx */

typedef struct RJSON_callback_ctx_struct {
	_type_pool *type_pool;		/* all JSON_types added here */
	_value_pool *value_pool;	/* int, float values added here */
	_r_pool r_pool;				/* CHARXSP, partial results added here */
	SEXPTYPE curr_rtype;		/* R type of current array / object */
	int curr_n;					/* length of current array / object */
} RJSON_callback_ctx;

RJSON_callback_ctx *
_RJSON_callback_ctx_new()
{
	/* car: stack; cdr: tree */
	RJSON_callback_ctx *ctx = Calloc(1, RJSON_callback_ctx);
	ctx->type_pool = _type_pool_new(NULL);
	ctx->value_pool = _value_pool_new(NULL);
	ctx->r_pool = _r_pool_new(); /* not PROTECT'ed */
	return ctx;
}

void
_RJSON_callback_ctx_delete(RJSON_callback_ctx *ctx)
{
	_type_pool *types = ctx->type_pool;
	while (types != NULL)
		types = _type_pool_delete(types);
	_value_pool *values = ctx->value_pool;
	while (values != NULL)
		values = _value_pool_delete(values);
	Free(ctx);
}

_r_pool_value
_RJSON_callback_ctx_r_get(RJSON_callback_ctx *ctx)
{
	return _r_pool_get(ctx->r_pool);
}

/* handler */

SEXPTYPE
_update_type(const SEXPTYPE curr, const SEXPTYPE next)
{
	SEXPTYPE value = curr;
	if (NILSXP == curr)
		value = next;
	else if (!((curr == next) || (NILSXP == next))) {
		switch(curr) {
		case INTSXP:
			if (next == REALSXP) value = next;
			else value = VECSXP;
			break;
		case REALSXP:
			if (next != INTSXP) value = VECSXP;
			break;
		default:
			value = VECSXP;
			break;
		}
	}
	return value;
}

void
_RJSON_handle_array_end(RJSON_callback_ctx *ctx)
{
	int curr_n = ctx->curr_n;
	SEXPTYPE curr_rtype = ctx->curr_rtype;

	SEXP rvalue = R_NilValue;
	JSON_value value;
	JSON_type type;

	switch(curr_rtype) {
	case NILSXP:
		if (curr_n == 0)
			rvalue = PROTECT(NEW_LIST(0));
		else {
			rvalue = PROTECT(NEW_LOGICAL(curr_n));
			while (0 != curr_n)
				LOGICAL(rvalue)[--curr_n] = NA_LOGICAL;
		}
		break;
	case LGLSXP:
		rvalue = PROTECT(NEW_LOGICAL(curr_n));
		while (0 != curr_n) {
			type = _type_pool_pop(&(ctx->type_pool));
			if (JSON_T_TRUE == type)
				LOGICAL(rvalue)[--curr_n] = TRUE;
			else if (JSON_T_FALSE == type)
				LOGICAL(rvalue)[--curr_n] = FALSE;
			else
				LOGICAL(rvalue)[--curr_n] = NA_LOGICAL;
		}
		break;
	case INTSXP:
		rvalue = PROTECT(NEW_INTEGER(curr_n));
		while (0 != curr_n) {
			type = _type_pool_pop(&(ctx->type_pool));
			if (JSON_T_INTEGER == type) {
				value = _value_pool_pop(&(ctx->value_pool));
				INTEGER(rvalue)[--curr_n] = value.vu.integer_value;
			} else {
				INTEGER(rvalue)[--curr_n] = NA_INTEGER;
			}
		}
		break;
	case REALSXP:
		rvalue = PROTECT(NEW_NUMERIC(curr_n));
		while (0 != curr_n) {
			type = _type_pool_pop(&(ctx->type_pool));
			if (JSON_T_FLOAT == type) {
				value = _value_pool_pop(&(ctx->value_pool));
				REAL(rvalue)[--curr_n] = value.vu.float_value;
			} else if (JSON_T_INTEGER == type) {
				value = _value_pool_pop(&(ctx->value_pool));
				REAL(rvalue)[--curr_n] = (double) value.vu.integer_value;
			} else {
				REAL(rvalue)[--curr_n] = NA_REAL;
			}
		}
		break;
	case STRSXP:
		rvalue = PROTECT(NEW_CHARACTER(curr_n));
		while (0 != curr_n) {
			type = _type_pool_pop(&(ctx->type_pool));
			if (JSON_T_STRING == type) {
				SET_STRING_ELT(rvalue, --curr_n,
							   _r_pool_pop(ctx->r_pool));
			} else {
				SET_STRING_ELT(rvalue, --curr_n, NA_STRING);
			}
		}
		break;
	case VECSXP:
		rvalue = PROTECT(NEW_LIST(curr_n));
		while (0 != curr_n) {
			type = _type_pool_pop(&(ctx->type_pool));
			switch (type) {
			case JSON_T_NULL:
				SET_VECTOR_ELT(rvalue, --curr_n,
							   ScalarLogical(NA_LOGICAL));
				break;
			case JSON_T_INTEGER:
				value = _value_pool_pop(&(ctx->value_pool));
				SET_VECTOR_ELT(rvalue, --curr_n,
							   ScalarInteger(value.vu.integer_value));
				break;
			case JSON_T_FLOAT:
				value = _value_pool_pop(&(ctx->value_pool));
				SET_VECTOR_ELT(rvalue, --curr_n,
							   ScalarReal(value.vu.float_value));
				break;
			case JSON_T_STRING:
				SET_VECTOR_ELT(rvalue, --curr_n,
							   ScalarString(_r_pool_pop(ctx->r_pool)));
				break;
			case JSON_T_ARRAY_BEGIN:
			case JSON_T_OBJECT_BEGIN:
				SET_VECTOR_ELT(rvalue, --curr_n,
							   _r_pool_pop(ctx->r_pool));
				break;
			default:
				assert(0);
				Rf_error("internal error: _RJSON_handle_array_end curr_n");
				break;
			}
		}
		break;
	default:
		assert(0);
		Rf_error("internal error: _RJSON_handle_array_end");
		break;
	}
	ctx->r_pool = _r_pool_push(ctx->r_pool, rvalue);
	/* restore / update nested count */
	ctx->curr_n = _value_pool_pop(&(ctx->value_pool)).vu.integer_value + 1;
	ctx->curr_rtype = VECSXP;
	UNPROTECT(1);
}

void
_RJSON_handle_object_end(RJSON_callback_ctx *ctx)
{
	int curr_n = ctx->curr_n;

	SEXP rvalue, rname, rtmp = R_NilValue;
	JSON_type type;
	JSON_value value;

	/* pop off as value_n, key_n, value_n-1, key_n-1, ... */
	rvalue = PROTECT(NEW_LIST(curr_n));
	rname = PROTECT(NEW_CHARACTER(curr_n));
	while (0 != curr_n) {
		--curr_n;
		type = _type_pool_pop(&(ctx->type_pool));
		switch ((JSON_type) type) {
		case JSON_T_NULL:
			rtmp = PROTECT(ScalarLogical(NA_LOGICAL));
			break;
		case JSON_T_TRUE:
			rtmp = PROTECT(ScalarLogical(TRUE));
			break;
		case JSON_T_FALSE:
			rtmp = PROTECT(ScalarLogical(FALSE));
			break;
		case JSON_T_INTEGER:
			value = _value_pool_pop(&(ctx->value_pool));
			rtmp = PROTECT(ScalarInteger(value.vu.integer_value));
			break;
		case JSON_T_FLOAT:
			value = _value_pool_pop(&(ctx->value_pool));
			rtmp = PROTECT(ScalarReal(value.vu.float_value));
			break;
		case JSON_T_STRING:
			rtmp = PROTECT(ScalarString(_r_pool_pop(ctx->r_pool)));
			break;
		case JSON_T_OBJECT_BEGIN:
		case JSON_T_ARRAY_BEGIN:
			rtmp = PROTECT(_r_pool_pop(ctx->r_pool));
			break;
		default:
			assert(0);
			Rf_error("internal error: _RJSON_handle_object_end");
		}
		SET_VECTOR_ELT(rvalue, curr_n, rtmp);
		SET_STRING_ELT(rname, curr_n, _r_pool_pop(ctx->r_pool));
		UNPROTECT(1);
	}

	SET_NAMES(rvalue, rname);
	ctx->r_pool = _r_pool_push(ctx->r_pool, rvalue);
	/* restore / update nested count  */
	ctx->curr_n = _value_pool_pop(&(ctx->value_pool)).vu.integer_value + 1;
	ctx->curr_rtype = VECSXP;
	UNPROTECT(2);
}

int
_RJSON_handler(void *vctx, int type,
			   const struct JSON_value_struct *value)
{
	RJSON_callback_ctx *ctx = (RJSON_callback_ctx *) vctx;
	Rboolean type_push = TRUE;
	switch((JSON_type) type) {
	case JSON_T_NULL:
		ctx->curr_rtype = _update_type(ctx->curr_rtype, NILSXP);
		ctx->curr_n += 1;
		break;
	case JSON_T_TRUE:
	case JSON_T_FALSE:
		ctx->curr_rtype = _update_type(ctx->curr_rtype, LGLSXP);
		ctx->curr_n += 1;
		break;
	case JSON_T_INTEGER:
		ctx->curr_rtype = _update_type(ctx->curr_rtype, INTSXP);
		ctx->value_pool = _value_pool_push(ctx->value_pool, value);
		ctx->curr_n += 1;
		break;
	case JSON_T_FLOAT:
		ctx->curr_rtype = _update_type(ctx->curr_rtype, REALSXP);
		ctx->value_pool = _value_pool_push(ctx->value_pool, value);
		ctx->curr_n += 1;
		break;
	case JSON_T_STRING:
		ctx->curr_rtype = _update_type(ctx->curr_rtype, STRSXP);
		ctx->r_pool =
			_r_pool_push(ctx->r_pool, mkChar(value->vu.str.value));
		ctx->curr_n += 1;
		break;
	case JSON_T_KEY:
		ctx->r_pool =
			_r_pool_push(ctx->r_pool, mkChar(value->vu.str.value));
		/* element counter incremented by value */
		type_push = FALSE;
		break;
	case JSON_T_ARRAY_BEGIN:
	case JSON_T_OBJECT_BEGIN: {
		/* push currrent length; use when popped */
		JSON_value tmp;
		tmp.vu.integer_value = ctx->curr_n;
		ctx->value_pool = _value_pool_push(ctx->value_pool, &tmp);
		ctx->curr_rtype = NILSXP;
		ctx->curr_n = 0;
	}
		break;
	case JSON_T_ARRAY_END:
		_RJSON_handle_array_end(ctx);
		type_push = FALSE;
		break;
	case JSON_T_OBJECT_END:
		_RJSON_handle_object_end(ctx);
		type_push = FALSE;
		break;
	default:
		assert(0);
		Rf_error("internal error: _RJSON_handler");
		break;
	}
	if (type_push)
		ctx->type_pool = _type_pool_push(ctx->type_pool, type);
	return 1;
}

/* parser */

typedef struct RJSON_parser_struct {
	JSON_parser json_parser;
	RJSON_callback_ctx *ctx;
} RJSON_parser;

RJSON_parser *
_RJSON_parser_new()
{
	RJSON_parser *rjson_parser = Calloc(1, RJSON_parser);
	rjson_parser->ctx = _RJSON_callback_ctx_new();
	PROTECT(rjson_parser->ctx->r_pool);

	JSON_config *config = Calloc(1, JSON_config);
	init_JSON_config(config);
    config->depth                  = 20;
    config->callback               = &_RJSON_handler;
	config->callback_ctx           = (void *) rjson_parser->ctx;
    config->allow_comments         = 1;
    config->handle_floats_manually = 0;
	rjson_parser->json_parser = new_JSON_parser(config);

	Free(config);
	UNPROTECT(1);
	return rjson_parser;
}

void
_RJSON_parser_delete(RJSON_parser *rjson_parser)
{
	RJSON_callback_ctx *ctx = rjson_parser->ctx;
	if (NULL == ctx)
		Rf_error("unexpected NULL ctx in _RJSON_parser_delete");
	_RJSON_callback_ctx_delete(ctx);

	JSON_parser json_parser = rjson_parser->json_parser;
	if (NULL == json_parser)
		Rf_error("unexpected NULL json_parser in _RJSON_parser_delete");
	delete_JSON_parser(json_parser);
	Free(rjson_parser);
}

void
_parser_delete(SEXP parser)
{
	RJSON_parser *rjson_parser =
		(RJSON_parser *) R_ExternalPtrAddr(parser);
	if (NULL == rjson_parser)
		return;
	_RJSON_parser_delete(rjson_parser);
	R_SetExternalPtrAddr(parser, NULL);
	R_SetExternalPtrTag(parser, R_NilValue);
	R_SetExternalPtrProtected(parser, R_NilValue); /* drop protection */
}

/* main interface; see RJSON_parser.h */

int
_JSON_to_R(JSON_parser jc, const char *s)
{
	for (int i = 0; ; ++i) {
		int next_char = (int) s[i];
		if (next_char <= 0)
			break;
		if (!JSON_parser_char(jc, next_char))
			return i + 1;
	}
	return 0;
}


SEXP
parser_delete(SEXP parser)
{
	_parser_delete(parser);
	return R_NilValue;
}

SEXP
parser_new()
{
	RJSON_parser *rjson_parser = _RJSON_parser_new();
	_r_pool r_pool = PROTECT(rjson_parser->ctx->r_pool);

	SEXP parser =
		PROTECT(R_MakeExternalPtr((void *) rjson_parser,
								  mkChar("RJSON parser"), r_pool));
	R_RegisterCFinalizerEx(parser, _parser_delete, TRUE);
	UNPROTECT(2);
	return parser;
}

SEXP
parser_add(SEXP parser, SEXP string, SEXP restart)
{
	if (EXTPTRSXP != TYPEOF(parser))
		Rf_error("'parser' must be an external pointer");
	if (!IS_CHARACTER(string))
		Rf_error("'string' must be character()'");
	if (!(IS_LOGICAL(restart) && 1L == LENGTH(restart)))
		Rf_error("'restart' must be logical(1)");
	RJSON_parser *rjson_parser =
		(RJSON_parser *) R_ExternalPtrAddr(parser);
	if (NULL == rjson_parser) {
		if (FALSE == LOGICAL(restart)[0])
			Rf_error("NULL parser");
		rjson_parser = _RJSON_parser_new();
		_r_pool r_pool = PROTECT(rjson_parser->ctx->r_pool);
		R_SetExternalPtrAddr(parser, rjson_parser);
		R_SetExternalPtrProtected(parser, r_pool);
		UNPROTECT(1);
	}
	JSON_parser json_parser = rjson_parser->json_parser;

	for (int i = 0; i < LENGTH(string); ++i) {
		int err_c =
			_JSON_to_R(json_parser, CHAR(STRING_ELT(string, i)));
		if (0 != err_c) {
			(void) parser_delete(parser);
			UNPROTECT(1);
			Rf_error("invalid JSON, string %d, character %d: '%c'",
					 i + 1, err_c,
					 CHAR(STRING_ELT(string, i))[err_c - 1]);
		}
	}
	return parser;
}

SEXP
parser_finalize(SEXP parser)
{
	if (EXTPTRSXP != TYPEOF(parser))
		Rf_error("'parser' must be an external pointer");
	RJSON_parser *rjson_parser =
		(RJSON_parser *) R_ExternalPtrAddr(parser);
	if (NULL == rjson_parser)
		Rf_error("NULL parser");
	int result = JSON_parser_done(rjson_parser->json_parser);
	if (0 == result)
		Rf_error("invalid or incomplete JSON stream");
	SEXP answer =
		PROTECT(_RJSON_callback_ctx_r_get(rjson_parser->ctx));
	(void) parser_delete(parser);
	UNPROTECT(1);
	return answer;
}

SEXP
JSON_to_R(SEXP string)
{
	if (!IS_CHARACTER(string))
		Rf_error("'string' must be character()'");
	if (1 == LENGTH(string) &&
		0 == strlen(CHAR(STRING_ELT(string, 0))))
		return string;
	SEXP parser = PROTECT(parser_new());
	SEXP restart = PROTECT(ScalarLogical(FALSE));
	parser = parser_add(parser, string, restart);
	SEXP result = PROTECT(parser_finalize(parser));
	UNPROTECT(3);
	return result;
}
