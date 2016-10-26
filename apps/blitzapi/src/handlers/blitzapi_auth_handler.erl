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
-module (blitzapi_auth_handler).
-author ("DaFew <1ofdafew@gmail.com>").
-include ("blitzapi.hrl").
-vsn("1.0.2").

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
        ?ERROR("Invalid username/password"),
        Reply = [{error, <<"Invalid username, or password">>}],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        {false, Req2, State}
    end
  catch
    _:badarg ->
      ?ERROR("Authentication error: Missing JSON data..."),
      Error = [{error, <<"Missing json data">>},
               {server_time, iso8601:format(Now)}],
      Req3 = cowboy_req:set_resp_body(jsx:encode(Error), Req),
      {false, Req3, State};
    _:Else ->
      ?ERROR("Authentication error: ~p", [Else]),
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
