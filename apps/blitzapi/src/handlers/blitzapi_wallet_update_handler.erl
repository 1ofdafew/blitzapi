-module (blitzapi_wallet_update_handler).
-include ("blitzapi.hrl").

-export ([init/3]).
-export ([allowed_methods/2]).
-export ([resource_exists/2]).
-export ([delete_resource/2]).
-export ([delete_completed/2]).
-export ([content_types_provided/2]).
-export ([content_types_accepted/2]).

-export ([get_wallet/2]).
-export ([do_transfer/2]).
-export ([ensure_exists/2]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, State) ->
  try
    {Wallet, Req1} = cowboy_req:binding(wallet, Req),
    {ok, {path, Dir}} = application:get_env(blitzapi, wallet_dir),
    File = <<Dir/binary, "/", Wallet/binary>>,
    case file:list_dir(binary_to_list(File)) of
      {error, enotdir} ->
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
    F1 = <<Dir/binary, "/", Wallet/binary>>,
    F2 = <<Dir/binary, "/archive/", Wallet/binary>>,
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
    F2 = <<Dir/binary, "/archive/", Wallet/binary>>,
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
  Now = erlang:timestamp(),
  try
    {Wallet, Req1} = cowboy_req:binding(wallet, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([<<"password">>], Data),
    Password = maps:get(<<"password">>, Data),

    Pid = blitzapi_port:start(open, {Wallet, Password}),
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
      {false, Req3, State};
    _:Else ->
      ?ERROR("Error: ~p", [Else]),
      Error = [{error, Else},
               {server_time, iso8601:format(Now)}],
      {jsx:encode(Error), Req, State}
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

    Cmd = <<"transfer ", Mixin/binary, " ", Address/binary, " ",
            Amount/binary, " -f ", Fee/binary>>,

    Pid = blitzapi_port:start(open, {Wallet, Password}),
    {ok, Result} = blitzapi_port:cmd(Pid, Cmd),
    blitzapi_port:stop(Pid),

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
