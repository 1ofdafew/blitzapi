%%%-------------------------------------------------------------------
%% @doc blitzapi public API
%% @end
%%%-------------------------------------------------------------------

-module(blitzapi_app).
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
      {<<"/api/v1/users">>, blitzapi_user_handler, []},
      {<<"/api/v1/users/:username">>, blitzapi_user_update_handler, []},
      {<<"/api/v1/wallets">>, blitzapi_wallet_handler, []},
      {<<"/api/v1/wallets/:wallet">>, blitzapi_wallet_update_handler, []}
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
