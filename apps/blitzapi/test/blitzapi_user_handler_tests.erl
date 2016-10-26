% Copyright (c) 2016, DaFew <1ofdafew@gmail.com>.
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:
%
% * Redistributions of source code must retain the above copyright
%   notice, this list of conditions and the following disclaimer.
%
% * Redistributions in binary form must reproduce the above copyright
%   notice, this list of conditions and the following disclaimer in the
%   documentation and/or other materials provided with the distribution.
%
% * The names of its contributors may not be used to endorse or promote
%   products derived from this software without specific prior written
%   permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
-module (blitzapi_user_handler_tests).
-author ("DaFew <1ofdafew@gmail.com>").
-vsn("1.0.1").

-include_lib("eunit/include/eunit.hrl").
-include ("blitzapi.hrl").

-compile(export_all).

blitzapi_user_handler_tests_() ->
  application:start(inets),
  application:start(blitzapi),

  {foreach, fun setup/0, fun teardown/1, [
    {with, [T]} || T <- [
      fun ?MODULE:user_using_get_test/0,
      fun ?MODULE:user_missing_ct_post_test/0,
      fun ?MODULE:user_missing_json_post_test/0,
      fun ?MODULE:user_missing_json_field_post_test/0
    ]]}.

setup() ->
  ok.

teardown(_Module) ->
  ok.

user_using_get_test() ->
  URL = "http://localhost:8443/api/v1/users",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  {Res, {{Version, RCode, _}, _Headers, _Body}} = httpc:request(get, {URL, Hdr}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 405).

user_missing_ct_post_test() ->
  URL = "http://localhost:8443/api/v1/users",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  {Res, {{Version, RCode, _}, _Headers, _Body}} = httpc:request(get, {URL, Hdr}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 405).

user_missing_json_post_test() ->
  URL = "http://localhost:8443/api/v1/users",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  CType = "application/json",
  {Res, {{Version, RCode, _}, _, _}} =
    httpc:request(post, {URL, Hdr, CType, ""}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 400).

user_missing_json_field_post_test() ->
  URL = "http://localhost:8443/api/v1/users",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  CType = "application/json",
  Body = jsx:encode([{username, <<"hisham">>}]),
  Data = binary_to_list(Body),
  {Res, {{Version, RCode, _}, _, _}} =
    httpc:request(post, {URL, Hdr, CType, Data}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 400).

user_complete_post_test() ->
  URL = "http://localhost:8443/api/v1/users",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  CType = "application/json",
  Body = jsx:encode([{username, <<"hisham">>}, {password, <<"sa">>}]),
  Data = binary_to_list(Body),
  {Res, {{Version, RCode, _}, _, _}} =
    httpc:request(post, {URL, Hdr, CType, Data}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 400).
