-module (blitzapi_user_handler).
-include ("blitzapi.hrl").

-import (blitzapi_user_update_handler, [to_user/1]).

-export ([init/3]).
-export ([allowed_methods/2]).
-export ([content_types_accepted/2]).
-export ([register_user/2]).
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
    {<<"application/json">>, register_user}
  ], Req, State}.

register_user(Req, State) ->
  Now = erlang:timestamp(),
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"username">>, <<"password">>], Data),

    Username = maps:get(<<"username">>, Data),
    ?INFO("Processing user creation for ~p...", [Username]),
    Password = maps:get(<<"password">>, Data),
    case blitzapi_db:register(Username, Password) of
      {ok, User} ->
        Reply = [{user, to_user(User)},
                 {server_time, iso8601:format(Now)}],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        {true, Req2, State};
      {error, _} ->
        Reply = [{error, <<"User exists">>},
                 {server_time, iso8601:format(Now)}],
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
