-module (blitzapi_auth_handler_tests).
-author ("DaFew <1ofdafew@gmail.com>").

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
      fun ?MODULE:user_missing_json_field_post_test/0,
      fun ?MODULE:complete_auth_post_test/0
    ]]}.

setup() ->
  ok.

teardown(_Module) ->
  ok.

user_using_get_test() ->
  URL = "http://localhost:8443/api/v1/auth",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  {Res, {{Version, RCode, _}, _Headers, _Body}} = httpc:request(get, {URL, Hdr}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 405).

user_missing_ct_post_test() ->
  URL = "http://localhost:8443/api/v1/auth",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  {Res, {{Version, RCode, _}, _Headers, _Body}} = httpc:request(post, {URL, Hdr, "", ""}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 415).

user_missing_json_post_test() ->
  URL = "http://localhost:8443/api/v1/auth",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  CType = "application/json",
  {Res, {{Version, RCode, _}, _, _}} =
    httpc:request(post, {URL, Hdr, CType, ""}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 400).

user_missing_json_field_post_test() ->
  URL = "http://localhost:8443/api/v1/auth",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  CType = "application/json",
  JSON = jsx:encode([{username, <<"hisham">>}]),
  Data = binary_to_list(JSON),
  {Res, {{Version, RCode, _}, _, _}} =
    httpc:request(post, {URL, Hdr, CType, Data}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 400).

complete_auth_post_test() ->
  URL = "http://localhost:8443/api/v1/auth",
  Hdr = [{"X-BlitzAPI-Key", "foo"}],
  CType = "application/json",
  JSON = jsx:encode([{username, <<"hisham">>}, {password, <<"sa">>}]),
  Data = binary_to_list(JSON),
  {Res, {{Version, RCode, _}, _, Body}} =
    httpc:request(post, {URL, Hdr, CType, Data}, [], []),

  ?assertEqual(Res, ok),
  ?assertEqual(Version, "HTTP/1.1"),
  ?assertEqual(RCode, 200),
  B1 = jsx:decode(list_to_binary(Body)),
  % [{<<"token">>, <<"HZGDK1ChHHssmPBkKDvXpzVh4YrAWedYXHge9nfGkaeC">>},
  %  {<<"server_time">>, <<"2016-10-23T16:34:10Z">>},
  %  {<<"valid_until">>, <<"2016-10-23T20:34:10Z">>}]

  ?assertEqual(3, length(B1)).
