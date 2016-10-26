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
-module (blitzapi_wallet_handler).
-author ("DaFew <1ofdafew@gmail.com>").
-include ("blitzapi.hrl").

-export ([init/3]).
-export ([allowed_methods/2]).
-export ([is_authorized/2]).
-export ([content_types_accepted/2]).
-export ([create_wallet/2]).
-export ([ensure_exists/2]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
  blitzapi_user_update_handler:is_authorized(Req, State).

% for POST
content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, create_wallet}
  ], Req, State}.

create_wallet(Req, State) ->
  Now = erlang:timestamp(),
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"wallet">>, <<"password">>], Data),

    Wallet = maps:get(<<"wallet">>, Data),
    Password = maps:get(<<"password">>, Data),
    ?INFO("Creating wallet ~p...", [Wallet]),

    % do some checking
    {ok, {path, Dir}} = application:get_env(blitzapi, wallet_dir),
    case file:list_dir(Dir) of
      {error, enoent} -> os:cmd("mkdir -p " ++ Dir)
    end,

    File = <<Dir/binary, "/", Wallet/binary>>,
    case file:list_dir(binary_to_list(File)) of
      {error, enoent} ->
        % ok, we can create the wallet
        Pid = blitzapi_port:start(new, {Wallet, Password}),
        timer:sleep(1000),
        {ok, Address} = blitzapi_port:cmd(Pid, <<"address">>),
        blitzapi_port:stop(Pid),

        ?INFO("Wallet created.."),
        % return result
        Reply = [Address, {server_time, iso8601:format(Now)}],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        {true, Req2, State};
      {error, enotdir} ->
        ?ERROR("Wallet error: wallet already exists..."),
        Reply = [{error, <<"Wallet exists">>},
                 {server_time, iso8601:format(Now)}],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        {false, Req2, State}
    end
  catch
    _:badarg ->
      ?ERROR("Wallet error: missing JSON data..."),
      Error = [{error, <<"Missing json data">>},
               {server_time, iso8601:format(Now)}],
      Req3 = cowboy_req:set_resp_body(jsx:encode(Error), Req),
      {false, Req3, State};
    _:Else ->
      ?ERROR("Wallet error: ~p", [Else]),
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
