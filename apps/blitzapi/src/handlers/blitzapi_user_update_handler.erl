-module (blitzapi_user_update_handler).
-include ("blitzapi.hrl").

-export ([init/3]).
-export ([allowed_methods/2]).
-export ([resource_exists/2]).
-export ([delete_resource/2]).
-export ([delete_completed/2]).
-export ([content_types_provided/2]).
-export ([content_types_accepted/2]).

-export ([get_user/2]).
-export ([update_user/2]).
-export ([ensure_exists/2]).
-export ([to_user/1]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, State) ->
  try
    {Username, Req1} = cowboy_req:binding(username, Req),
    ?INFO("Resource: ~p", [Username]),
    case blitzapi_db:get({blitzapi_users, Username}) of
      {ok, [_User]} ->
        {true, Req1, State};
      {ok, []} ->
        {false, Req1, State}
    end
  catch
    _:_ ->
      {false, Req, State}
  end.

% for DELETE
delete_resource(Req, State) ->
  try
    {Username, Req1} = cowboy_req:binding(username, Req),
    case blitzapi_db:get({blitzapi_users, Username}) of
      {ok, [_User]} ->
        blitzapi_db:delete({blitzapi_users, Username}),
        {true, Req1, State};
      {ok, []} ->
        {true, Req1, State}
    end
  catch
    _:_ ->
      {false, Req, State}
  end.

delete_completed(Req, State) ->
  {true, Req, State}.

%  for GET
content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_user}], Req, State}.

% for PUT, PATCH
content_types_accepted(Req, State) ->
  {[{<<"application/json">>, update_user}], Req, State}.

get_user(Req, State) ->
  Now = erlang:timestamp(),
  try
    {Username, Req1} = cowboy_req:binding(username, Req),
    case blitzapi_db:get({blitzapi_users, Username}) of
      {ok, [User]} ->
        Reply = [{user, to_user(User)},
                 {server_time, iso8601:format(Now)}],
        {jsx:encode(Reply), Req1, State};
      {ok, []} ->
        Reply = [{error, <<"No such user">>},
                 {server_time, iso8601:format(Now)}],
        {jsx:encode(Reply), Req1, State}
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
      {jsx:encode(Error), Req, State}
  end.

update_user(Req, State) ->
  Now = erlang:timestamp(),
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"username">>,
                   <<"password">>,
                   <<"created">>,
                   <<"updated">>,
                   <<"active">>], Data),

    Username = maps:get(<<"username">>, Data),
    ?INFO("Processing user creation for ~p...", [Username]),
    Password = maps:get(<<"password">>, Data),
    Created = maps:get(<<"created">>, Data),
    Updated = maps:get(<<"updated">>, Data),
    Active = maps:get(<<"active">>, Data),

    Pass = base58:encode(crypto:hash(sha256, Password)),
    User = #blitzapi_users{username=Username,
                           password=Pass,
                           created=Created,
                           updated=Updated,
                           active=Active},

    blitzapi_db:put(User),
    Reply = [{user, User},
             {server_time, iso8601:format(Now)}],
    Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
    {true, Req2, State}
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

to_user(User) ->
  #blitzapi_users{username=Username,
                  created=Created,
                  updated=Updated,
                  active=Active} = User,
  [{username, Username},
   {password, <<"******">>},
   {created, Created},
   {updated, Updated},
   {active, Active}].
