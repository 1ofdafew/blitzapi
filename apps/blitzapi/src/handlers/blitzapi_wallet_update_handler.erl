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
-module (blitzapi_wallet_update_handler).
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

-export ([get_wallet/2]).
-export ([do_transfer/2]).
-export ([ensure_exists/2]).

-define (SLEEP, 200).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
  blitzapi_user_update_handler:is_authorized(Req, State).

resource_exists(Req, State) ->
  try
    {Wallet, Req1} = cowboy_req:binding(wallet, Req),
    {ok, {path, Dir}} = application:get_env(blitzapi, wallet_dir),
    File = <<Dir/binary, "/", Wallet/binary, ".wallet">>,
    case file:list_dir(binary_to_list(File)) of
      {error, enotdir} ->
        ?INFO("Wallet ~p exists...", [Wallet]),
        {true, Req1, State};
      _ ->
        {false, Req1, State}
    end
  catch
    _:_ ->
      {false, Req, State}
  end.

% for DELETE
delete_resource(Req, State) ->
  try
    {Wallet, Req1} = cowboy_req:binding(wallet, Req),
    {ok, {path, Dir}} = application:get_env(blitzapi, wallet_dir),
    D1 = <<Dir/binary, "/archive">>,
    F1 = <<Dir/binary, "/", Wallet/binary, "*">>,
    F2 = <<Dir/binary, "/archive/.">>,
    case file:list_dir(binary_to_list(D1)) of
      {error, enoent} ->
        os:cmd("mkdir -p " ++ binary_to_list(D1)),
        Move = <<"mv ", F1/binary, " ", F2/binary>>,
        os:cmd(binary_to_list(Move));
      _ ->
        Move = <<"mv ", F1/binary, " ", F2/binary>>,
        os:cmd(binary_to_list(Move))
    end,
    {true, Req1, State}
  catch
    _:_ ->
      {false, Req, State}
  end.

delete_completed(Req, State) ->
  try
    {Wallet, Req1} = cowboy_req:binding(wallet, Req),
    {ok, {path, Dir}} = application:get_env(blitzapi, wallet_dir),
    F2 = <<Dir/binary, "/archive/", Wallet/binary, ".wallet">>,
    case file:list_dir(binary_to_list(F2)) of
      {error, enoent} ->
        {false, Req, State};
      _ ->
        {true, Req1, State}
    end
  catch
    _:_ ->
      {false, Req, State}
  end.

%  for GET
content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_wallet}], Req, State}.

% for PUT, PATCH
content_types_accepted(Req, State) ->
  {[{<<"application/json">>, do_transfer}], Req, State}.

get_wallet(Req, State) ->
  ?INFO("Getting wallet details..."),
  Now = erlang:timestamp(),
  try
    {Wallet, Req1} = cowboy_req:binding(wallet, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"password">>], Data),
    Password = maps:get(<<"password">>, Data),

    Pid = blitzapi_port:start(open, {Wallet, Password}),
    timer:sleep(?SLEEP),
    {ok, Balance, Locked} = blitzapi_port:cmd(Pid, <<"balance">>),
    {ok, Address} = blitzapi_port:cmd(Pid, <<"address">>),
    blitzapi_port:stop(Pid),

    % return result
    Reply = [Address, Balance, Locked,
             {server_time, iso8601:format(Now)}],
    {jsx:encode(Reply), Req2, State}
  catch
    _:badarg ->
      Error = [{error, <<"Missing json data">>},
               {server_time, iso8601:format(Now)}],
      Req3 = cowboy_req:set_resp_body(jsx:encode(Error), Req),
      {jsx:encode(Error), Req3, State};
    _:_ ->
      Error = [{error, <<"Invalid wallet, or bad password">>},
               {server_time, iso8601:format(Now)}],
      Req3 = cowboy_req:set_resp_body(jsx:encode(Error), Req),
      {jsx:encode(Error), Req3, State}
  end.

do_transfer(Req, State) ->
  Now = erlang:timestamp(),
  try
    {Wallet, Req1} = cowboy_req:binding(wallet, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"password">>,
                   <<"mixin">>,
                   <<"address">>,
                   <<"amount">>,
                   <<"fee">>], Data),

    Password = maps:get(<<"password">>, Data),
    Mixin = maps:get(<<"mixin">>, Data),
    Address = maps:get(<<"address">>, Data),
    Amount = maps:get(<<"amount">>, Data),
    Fee = maps:get(<<"fee">>, Data),
    ?INFO("Processing coin transfer from wallet ~p to address ~p, amount=~p, fee=~p",
      [Wallet, Address, Amount, Fee]),

    Cmd = io_lib:format("transfer ~p ~s ~p -f ~p", [Mixin, binary_to_list(Address), Amount, Fee]),
    Pid = blitzapi_port:start(open, {Wallet, Password}),
    timer:sleep(?SLEEP),
    {ok, Result} = blitzapi_port:cmd(Pid, iolist_to_binary(Cmd)),
    blitzapi_port:stop(Pid),

    ?INFO("Result of coin transfer: ~p", [Result]),

    Reply = [{result, Result},
             {server_time, iso8601:format(Now)}],
    Req3 = cowboy_req:set_resp_body(jsx:encode(Reply), Req2),
    {true, Req3, State}
  catch
    _:Else ->
      ?ERROR("Error: ~p", [Else]),
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
