%%%-------------------------------------------------------------------
%% @doc blitzapi top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(blitzapi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WORKER(Name, Svr, Args), {Name, {Svr, start_link, Args},
          temporary, 1000, worker, [Svr]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Db = ?WORKER(db, blitzapi_db, []),
  {ok, { {one_for_one, 60, 10}, [Db]} }.

%%====================================================================
%% Internal functions
%%====================================================================
