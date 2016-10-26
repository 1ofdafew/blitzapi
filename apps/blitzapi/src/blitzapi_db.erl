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
-module (blitzapi_db).
-author ("DaFew <1ofdafew@gmail.com>").
-behaviour (gen_server).
-vsn("1.0.1").

-include ("blitzapi.hrl").

-define (SERVER, ?MODULE).
-record (state, {}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([start_link/0]).
-export ([register/2]).
-export ([authenticate/2]).

-export ([get/1]).
-export ([put/1]).
-export ([delete/1]).

%% ------------------------------------------------------------------
%% Type Exports
%% ------------------------------------------------------------------
-type databases() :: [{ table(), fields() }].
-export_type([databases/0]).

-type table() :: atom().
-export_type([table/0]).

-type fields() :: term().
-export_type([fields/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register(Username :: binary(),
               Password :: binary()) -> ok.
register(Username, Password) ->
  gen_server:call(?SERVER, {register, Username, Password}).

-spec authenticate(Username :: binary(),
                   Password :: binary()) -> ok.
authenticate(Username, Password) ->
  gen_server:call(?SERVER, {authenticate, Username, Password}).

-spec get(Record :: term()) -> ok.
get(Record) ->
  gen_server:call(?SERVER, {get, Record}).

-spec put(Record :: term()) -> ok.
put(Record) ->
  gen_server:cast(?SERVER, {put, Record}).

-spec delete(Record :: term()) -> ok.
delete(Record) ->
  gen_server:cast(?SERVER, {delete, Record}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  ?INFO("~p starting...", [?MODULE]),
  try
    mnesia:dirty_read({blitzapi_users, <<"no_such_user">>})
  catch
    _:{aborted, _Error} ->
      % database not being setup
      DBs = [{blitzapi_users, record_info(fields, blitzapi_users)},
             {blitzapi_tokens, record_info(fields, blitzapi_tokens)}],
      setup_db(DBs, false)
  end,
  {ok, #state{}}.

handle_call({register, Username, Password}, _From, State) ->
  Res = case mnesia:dirty_read({blitzapi_users, Username}) of
    [] ->
      TS = iso8601:format(erlang:timestamp()),
      U = #blitzapi_users{username=Username,
                          password=base58:encode(crypto:hash(sha256, Password)),
                          created=TS,
                          updated=TS,
                          active=true},
      ?INFO("User added: ~p", [U]),
      mnesia:dirty_write(U),
      {ok, U};
    _ ->
      {error, username_taken}
  end,
  {reply, Res, State};

handle_call({authenticate, Username, Password}, _From, State) ->
  ?INFO("Authenticating user ~p...", [Username]),
  Reply = case mnesia:dirty_read({blitzapi_users, Username}) of
    [#blitzapi_users{password=P0}] ->
      Pass = base58:encode(crypto:hash(sha256, Password)),
      % ?INFO("P0 = ~p, Pass = ~p", [P0, Pass]),
      case Pass =:= P0 of
        true ->
          {ok, proceed};
        _ ->
          {error, bad_user}
      end;
    [] ->
      {error, bad_user}
  end,
  % ?INFO("Reply = ~p", [Reply]),
  {reply, Reply, State};

handle_call({get, Record}, _From, State) ->
  User = mnesia:dirty_read(Record),
  {reply, {ok, User}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({put, Record}, State) ->
  mnesia:dirty_write(Record),
  {noreply, State};

handle_cast({delete, Record}, State) ->
  mnesia:dirty_delete(Record),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec setup_db(Databases :: databases(),
               Reset :: boolean()) -> ok.
setup_db(Databases, Reset) ->
  % we can choose mnesia, or mnesia_ext
  application:stop(mnesia),
  case Reset of
    true -> mnesia:delete_schema([node()]);
    _    -> ok
  end,
  mnesia:create_schema([node()]),
  ok = application:start(mnesia),
  create(Databases).

-spec create(Databases :: list()) -> ok.
create([{Table, Fields}|Rest]) ->
  create_table(Table, Fields),
  create(Rest);
create([]) -> ok.

-spec create_table(Tab::atom(),
                 Fields::list()) -> ok.
create_table(Tab, Fields) ->
  ?INFO("Creating table ~p...", [Tab]),
  mnesia:create_table(Tab, [{attributes, Fields},
                            {disc_copies, [node()]},
                            {type, ordered_set}]),
  mnesia:wait_for_tables([Tab], 1000).
