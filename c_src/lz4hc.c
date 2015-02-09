/* lz4hc.c -- wrapper of the LZ4HC algorithm

   This file is part of the erlang elz4 data compression library.

   Copyright (C) 2015 Oleksandr Chumachenko
   All Rights Reserved.

   The elz4 library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   The elz4 library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with the elz4 library; see the file COPYING.
   If not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

   Oleksandr Chumachenko
   <ledest@gmail.com>
   https://github.com/Ledest/elz4/
 */

#include <erl_nif.h>
#include <lz4.h>
#include <lz4hc.h>
#include <stdint.h>

static inline ERL_NIF_TERM make_ok(ErlNifEnv *env, ERL_NIF_TERM msg)
{
	return enif_make_tuple2(env, enif_make_atom(env, "ok"), msg);
}

static inline ERL_NIF_TERM make_error(ErlNifEnv *env, const char *msg)
{
	return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, msg));
}

static ERL_NIF_TERM lz4hc_zip(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i, o;
	int os;
	const char *s = "insufficient_memory";

	if (!enif_inspect_binary(env, argv[0], &i) && !enif_inspect_iolist_as_binary(env, argv[0], &i))
		return enif_make_badarg(env);

	if (!enif_alloc_binary(LZ4_compressBound(i.size), &o))
		return make_error(NULL, s);

	os = LZ4_compressHC((const char*)i.data, (char*)o.data, i.size);
	if (os) {
		if (os == o.size || enif_realloc_binary(&o, os))
			return enif_make_binary(env, &o);
		s = "unknown";
	}
	return make_error(env, s);
}

static ERL_NIF_TERM lz4hc_compress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i, o;
	int os;
	const char *s = "insufficient_memory";

	if (!enif_inspect_binary(env, argv[0], &i) || !enif_inspect_iolist_as_binary(env, argv[0], &i))
		return enif_make_badarg(env);

	if (!enif_alloc_binary(LZ4_compressBound(i.size) + sizeof(uint32_t), &o))
		return make_error(NULL, s);

	*(uint32_t*)o.data = (uint32_t)i.size;
	os = LZ4_compressHC((const char*)i.data, (char*)o.data + sizeof(uint32_t), i.size);
	if (os) {
		if (os == o.size || enif_realloc_binary(&o, os + sizeof(uint32_t)))
			return make_ok(env, enif_make_binary(env, &o));
		s = "unknown";
	}
	return make_error(env, s);
}

static int on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
{
	return 0;
}

static int on_reload(ErlNifEnv *env, void**priv, ERL_NIF_TERM info)
{
	return 0;
}

static int on_upgrade(ErlNifEnv *env, void **priv, void** old_priv, ERL_NIF_TERM info)
{
	return 0;
}


static ErlNifFunc nif_functions[] = {
	{"zip", 1, lz4hc_zip},
	{"compress", 1, lz4hc_compress},
};

ERL_NIF_INIT(lz4hc, nif_functions, &on_load, &on_reload, &on_upgrade, NULL);
