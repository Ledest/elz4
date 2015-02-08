%% lz4_test.erl -- unit test for wrapper of the elz4 library.
%%
%% This file is part of the erlang elz4 data compression library.
%% Copyright (C) 2015 Oleksandr Chumachenko
%% All Rights Reserved.
%%
%% The elz4 library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License as
%% published by the Free Software Foundation; either version 2 of
%% the License, or (at your option) any later version.
%%
%% The elz4 library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License
%% along with the elz4 library; see the file COPYING.
%% If not, write to the Free Software Foundation, Inc.,
%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
%%
%% Oleksandr Chumachenko
%% <ledest@gmail.com>
%% https://github.com/Ledest/elz4/

-module(lz4_tests).
-include_lib("eunit/include/eunit.hrl").

lz4_test_() ->
    {timeout, 60, [fun lz4/0]}.

lz4hc_test_() ->
    {timeout, 60, [fun lz4hc/0]}.

lz4() ->
    {ok, Data} = file:read_file("../README.md"),
    io:fwrite("~p~n", [file:get_cwd()]),
    Compressed1 = lz4:zip(Data),
    Decompressed1 = lz4:unzip(Compressed1, size(Data) * 256),
    Decompressed2 = lz4:unzip(Compressed1, size(Data)),
    Decompressed3 = lz4:unzip(Compressed1),
    {ok, Compressed4} = lz4:compress(Data),
    {ok, Decompressed4} = lz4:decompress(Compressed4),
    ?assertEqual(Data, Decompressed1),
    ?assertEqual(Data, Decompressed2),
    ?assertEqual(Data, Decompressed3),
    ?assertEqual(Data, Decompressed4).

lz4hc() ->
    {ok, Data} = file:read_file("../README.md"),
    io:fwrite("~p~n", [file:get_cwd()]),
    Compressed1 = lz4hc:zip(Data),
    Decompressed1 = lz4:unzip(Compressed1, size(Data) * 256),
    Decompressed2 = lz4:unzip(Compressed1, size(Data)),
    Decompressed3 = lz4:unzip(Compressed1),
    {ok, Compressed4} = lz4hc:compress(Data),
    {ok, Decompressed4} = lz4:decompress(Compressed4),
    ?assertEqual(Data, Decompressed1),
    ?assertEqual(Data, Decompressed2),
    ?assertEqual(Data, Decompressed3),
    ?assertEqual(Data, Decompressed4).
