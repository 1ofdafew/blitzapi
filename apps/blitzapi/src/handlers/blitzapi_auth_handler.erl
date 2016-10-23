-module (blitzapi_auth_handler).
-include ("blitzapi.hrl").

-export ([init/3]).
-export ([allowed_methods/2]).
-export ([content_types_accepted/2]).
-export ([process_auth/2]).
-export ([ensure_exists/2]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

% for POST
content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, process_auth}
  ], Req, State}.

process_auth(Req, State) ->
  Now = {MM, M, S} = erlang:timestamp(),
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"username">>, <<"password">>], Data),

    Username = maps:get(<<"username">>, Data),
    ?INFO("Processing user authorization for ~p...", [Username]),
    Password = maps:get(<<"password">>, Data),
    case blitzapi_db:authenticate(Username, Password) of
      {ok, proceed} ->
        % generate token for next calls
        Token = base58:encode(crypto:hash(sha256, crypto:strong_rand_bytes(12))),
        ?INFO("Generating user token ~p...", [Token]),

        % save the token
        Valid = {MM, M + 4*60*60, S}, %% give 4-hour time limit
        Tok = #blitzapi_tokens{token=Token,
                               created=Now,
                               valid_until=Valid},
        blitzapi_db:put(Tok),

        Reply = [{token, Token},
                 {server_time, iso8601:format(Now)},
                 {valid_until, iso8601:format(Valid)}],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        {true, Req2, State};
      _ ->
        Reply = [{error, <<"Invalid username, or password">>}],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        {false, Req2, State}
    end
  catch
    _:badarg ->
      Error = [{error, <<"Missing json data">>},
               {server_time, iso8601:format(Now)}],
      Req3 = cowboy_req:set_resp_body(jsx:encode(Error), Req),
      {false, Req3, State};
    _:Else ->
      ?ERROR("Error: ~p", [Else]),
      Error = [{error, Else},
               {server_time, iso8601:format(Now)}],
      Req3 = cowboy_req:set_resp_body(jsx:encode(Error), Req),
      {false, Req3, State}
  end.

ensure_exists([H|T], Map) ->
  case maps:find(H, Map) of
    error ->
      ?ERROR("Param ~p is missing", [H]),
      throw(<<"Parameter ", H/binary, " is mandatory">>);
    _ ->
      ensure_exists(T, Map)
  end;
ensure_exists([], _Map) -> ok.
