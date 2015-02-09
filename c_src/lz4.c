/* lz4.c -- wrapper of the LZ4 algorithm

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
#include <stdint.h>

static inline ERL_NIF_TERM make_ok(ErlNifEnv *env, ERL_NIF_TERM msg)
{
	return enif_make_tuple2(env, enif_make_atom(env, "ok"), msg);
}

static inline ERL_NIF_TERM make_error(ErlNifEnv *env, const char *msg)
{
	return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, msg));
}

ERL_NIF_TERM lz4_zip(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i, o;
	int os;
	const char *s = "insufficient_memory";

	if (!enif_inspect_binary(env, argv[0], &i) && !enif_inspect_iolist_as_binary(env, argv[0], &i))
		return enif_make_badarg(env);

	if (!enif_alloc_binary(LZ4_compressBound(i.size), &o))
		return make_error(NULL, s);

	os = LZ4_compress((const char*)i.data, (char*)o.data, i.size);
	if (os) {
		if (os == o.size || enif_realloc_binary(&o, os))
			return enif_make_binary(env, &o);
		s = "unknown";
	}
	return make_error(env, s);
}

static ERL_NIF_TERM lz4_unzip(ErlNifEnv* env, ErlNifBinary *i, int ms)
{
	ErlNifBinary o;
	int os;

	if (!enif_alloc_binary(ms, &o))
		return make_error(NULL, "insufficient_memory");

	os = LZ4_decompress_safe((const char*)i->data, (char*)o.data, i->size, o.size);
	return os < 0
	       ? make_error(env, "unknown")
	       : (os == o.size || enif_realloc_binary(&o, os))
	         ? enif_make_binary(env, &o)
	         : make_error(env, "insufficient_memory");
}

ERL_NIF_TERM lz4_unzip_2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i;
	int ms;

	return (enif_get_int(env, argv[1], &ms) && enif_inspect_iolist_as_binary(env, argv[0], &i))
	       ? lz4_unzip(env, &i, ms)
	       : enif_make_badarg(env);
}

ERL_NIF_TERM lz4_unzip_1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i;

	return (enif_inspect_binary(env, argv[0], &i) || enif_inspect_iolist_as_binary(env, argv[0], &i))
	       ? lz4_unzip(env, &i, i.size * 256)
	       : enif_make_badarg(env);
}

ERL_NIF_TERM lz4_compress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i, o;
	int os;
	const char *s = "insufficient_memory";

	if (!enif_inspect_binary(env, argv[0], &i) || !enif_inspect_iolist_as_binary(env, argv[0], &i))
		return enif_make_badarg(env);

	if (!enif_alloc_binary(LZ4_compressBound(i.size) + sizeof(uint32_t), &o))
		return make_error(NULL, s);

	*(uint32_t*)o.data = (uint32_t)i.size;
	os = LZ4_compress((const char*)i.data, (char*)o.data + sizeof(uint32_t), i.size);
	if (os) {
		if (os == o.size || enif_realloc_binary(&o, os + sizeof(uint32_t)))
			return make_ok(env, enif_make_binary(env, &o));
		s = "unknown";
	}
	return make_error(env, s);
}

ERL_NIF_TERM lz4_decompress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary i, o;
	int os;

	if (!enif_inspect_binary(env, argv[0], &i) || !enif_inspect_iolist_as_binary(env, argv[0], &i))
		return enif_make_badarg(env);

	if (!enif_alloc_binary(*(uint32_t*)i.data, &o))
		return make_error(NULL, "insufficient_memory");

	os = LZ4_decompress_fast((const char*)i.data + sizeof(uint32_t), (char*)o.data, o.size);
	return os >= 0
	       ? make_ok(env, enif_make_binary(env, &o))
	       : make_error(env, "unknown");
}

int on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
{
	return 0;
}

int on_reload(ErlNifEnv *env, void**priv, ERL_NIF_TERM info)
{
	return 0;
}

int on_upgrade(ErlNifEnv *env, void **priv, void** old_priv, ERL_NIF_TERM info)
{
	return 0;
}


static ErlNifFunc nif_functions[] = {
	{"zip", 1, lz4_zip},
	{"unzip", 2, lz4_unzip_2},
	{"unzip", 1, lz4_unzip_1},
	{"compress", 1, lz4_compress},
	{"uncompress", 1, lz4_decompress},
	{"decompress", 1, lz4_decompress}
};

ERL_NIF_INIT(lz4, nif_functions, &on_load, &on_reload, &on_upgrade, NULL);
