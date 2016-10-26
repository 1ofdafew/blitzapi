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
-module (blitzapi_user_update_handler).
-author ("DaFew <1ofdafew@gmail.com>").
-include ("blitzapi.hrl").

-export ([init/3]).
-export ([allowed_methods/2]).
-export ([is_authorized/2]).
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

is_authorized(Req, State) ->
  {_, Auth, Req1} = cowboy_req:parse_header(<<"x-blitzcoin-key">>, Req),
  case blitzapi_db:get({blitzapi_tokens, Auth}) of
    {ok, [_Val]} ->
      % validate token
      ?INFO("Token ~p is validated", [Auth]),
      {true, Req1, Auth};
    {ok, []} ->
      {{false, <<"Basic Realm=\"Blitzcoin API V1.0.0\"">>}, Req1, State}
  end.

resource_exists(Req, State) ->
  try
    {Username, Req1} = cowboy_req:binding(username, Req),
    case blitzapi_db:get({blitzapi_users, Username}) of
      {ok, [_User]} ->
        ?INFO("Resource ~p exists....", [Username]),
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
    ?INFO("Processing user delete for ~p...", [Username]),

    {ok, Body, Req2} = cowboy_req:body(Req1),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"password">>], Data),
    Password = maps:get(<<"password">>, Data),

    case blitzapi_db:get({blitzapi_users, Username}) of
      {ok, [User]} ->
        case compare_passwd(User, Password) of
          true ->
            blitzapi_db:delete({blitzapi_users, Username}),
            ?INFO("User ~p is deleted.", [Username]),
            {true, Req2, State};
          false ->
            {false, Req2, State}
        end;
      {ok, []} ->
        {true, Req2, State}
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
    ?INFO("Getting user info for ~p", [Username]),

    {ok, Body, Req2} = cowboy_req:body(Req1),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"password">>], Data),
    Password = maps:get(<<"password">>, Data),

    case blitzapi_db:get({blitzapi_users, Username}) of
      {ok, [User]} ->
        case compare_passwd(User, Password) of
          true ->
            ?INFO("User ~p is validated...", [Username]),
            Reply = [{user, to_user(User)},
                     {server_time, iso8601:format(Now)}],
            {jsx:encode(Reply), Req1, State};
          false ->
            ?ERROR("Invalid user/password!"),
            Reply = [{error, <<"No such user, or bad password">>},
                     {server_time, iso8601:format(Now)}],
            {jsx:encode(Reply), Req1, State}
        end;
      {ok, []} ->
        ?ERROR("No such user..."),
        Reply = [{error, <<"No such user">>},
                 {server_time, iso8601:format(Now)}],
        {jsx:encode(Reply), Req1, State}
    end
  catch
    _:badarg ->
      ?ERROR("User error: missing JSON data"),
      Error = [{error, <<"Missing json data">>},
               {server_time, iso8601:format(Now)}],
      Req3 = cowboy_req:set_resp_body(jsx:encode(Error), Req),
      {false, Req3, State};
    _:Else ->
      ?ERROR("User error: ~p", [Else]),
      Error = [{error, Else},
               {server_time, iso8601:format(Now)}],
      {jsx:encode(Error), Req, State}
  end.

update_user(Req, State) ->
  Now = erlang:timestamp(),
  try
    {Username, Req1} = cowboy_req:binding(username, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"password">>,
                   <<"active">>], Data),

    ?INFO("Processing user update for ~p...", [Username]),
    Password = maps:get(<<"password">>, Data),
    Active = maps:get(<<"active">>, Data),
    Pass = base58:encode(crypto:hash(sha256, Password)),

    {ok, [U0]} = blitzapi_db:get({blitzapi_users, Username}),
    Created = U0#blitzapi_users.created,

    User = #blitzapi_users{username=Username,
                           password=Pass,
                           created=Created,
                           updated=iso8601:format(Now),
                           active=Active},

    blitzapi_db:put(User),
    ?INFO("User ~p details are updated...", [Username]),
    Reply = [{user, to_user(User)},
             {server_time, iso8601:format(Now)}],
    Req3 = cowboy_req:set_resp_body(jsx:encode(Reply), Req2),
    {true, Req3, State}
  catch
    _:badarg ->
      ?ERROR("User error: missing JSON data"),
      Error = [{error, <<"Missing json data">>},
               {server_time, iso8601:format(Now)}],
      Req4 = cowboy_req:set_resp_body(jsx:encode(Error), Req),
      {false, Req4, State};
    _:Else ->
      ?ERROR("User error: ~p", [Else]),
      Error = [{error, Else},
               {server_time, iso8601:format(Now)}],
      Req4 = cowboy_req:set_resp_body(jsx:encode(Error), Req),
      {false, Req4, State}
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

compare_passwd(User, Password) ->
  P1 = base58:encode(crypto:hash(sha256, Password)),
  User#blitzapi_users.password =:= P1.
