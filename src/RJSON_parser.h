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

#ifndef RJSON_PARSER_H
#define RJSON_PARSER_H

SEXP JSON_to_R(SEXP string);

SEXP parser_new();
SEXP parser_add(SEXP parser, SEXP string, SEXP restart);
SEXP parser_finalize(SEXP parser); /* return R object, delete parser */
SEXP parser_delete(SEXP parser);   /* delete without yield */

#endif
