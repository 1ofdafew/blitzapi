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
%%%-------------------------------------------------------------------
%% @doc blitzapi public API
%% @end
%%%-------------------------------------------------------------------

-module(blitzapi_app).
-author ("DaFew <1ofdafew@gmail.com>").
-behaviour(application).
-include ("blitzapi.hrl").

%% Application callbacks
-export([start/2]).
-export([stop/1]).
-export([start_phase/3]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  application:ensure_all_started(lager),
  application:ensure_all_started(cowboy),
  application:ensure_all_started(jsx),
  application:ensure_all_started(sync),
  blitzapi_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_phase(start_listeners, _StartType, []) ->
  ?INFO("Starting HTTP listeners..."),
  {ok, [{port, Port}, {listeners, Listeners}]} = application:get_env(blitzapi, http),
  Dispatch = cowboy_router:compile([
    {'_', [
      {<<"/api/v1/auth">>, blitzapi_auth_handler, []},
      {<<"/api/v1/users/:username">>, blitzapi_user_update_handler, []},
      {<<"/api/v1/users">>, blitzapi_user_handler, []},
      {<<"/api/v1/wallets/:wallet">>, blitzapi_wallet_update_handler, []},
      {<<"/api/v1/wallets">>, blitzapi_wallet_handler, []}
    ]}
  ]),
  RanchOptions = [{port, Port}],
  CowboyOptions = [
    {env, [{dispatch, Dispatch}]},
    {middlewares, [cowboy_router, cors_middleware, cowboy_handler]},
    {compress, true},
    {timeout, 12000}
  ],
  cowboy:start_http(blitzapi, Listeners, RanchOptions, CowboyOptions),
  ok.
