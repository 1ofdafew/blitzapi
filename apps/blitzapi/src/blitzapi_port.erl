-module(blitzapi_port).
-include ("blitzapi.hrl").

-export ([start/2]).
-export ([stop/1]).
-export ([cmd/2]).
-export ([init/2]).

start(Cmd, {Wallet, Password}) ->
  spawn(?MODULE, init, [Cmd, {Wallet, Password}]).

stop(Pid) ->
  Pid ! stop.

cmd(Pid, Command) ->
  Pid ! {self(), command, Command},
  receive
    Result -> Result
  after
    3000 ->
      timeout
  end.

init(Cmd, {Wallet, Password}) ->
  ?INFO("Starting for ~p...", [Wallet]),
  process_flag(trap_exit, true),
  {ok, {path, App}} = application:get_env(blitzapi, wallet_app),
  {ok, {path, Dir}} = application:get_env(blitzapi, wallet_dir),

  Args = case Cmd of
    new ->
      <<App/binary, " --generate-new-wallet ", Dir/binary, "/", Wallet/binary, " --password ", Password/binary>>;
    _ ->
      <<App/binary, " --wallet-file ", Dir/binary, "/", Wallet/binary, " --password ", Password/binary>>
  end,

  Port = open_port({spawn, binary_to_list(Args)}, [binary, {line, 512}]),
  loop(Port).

loop(Port) ->
  receive
    {From, command, <<"balance">> = Cmd} ->
      port_command(Port, <<Cmd/binary, "\r\n">>),
      From ! read_balance(Port);
    {From, command, <<"address">> = Cmd} ->
      port_command(Port, <<Cmd/binary, "\r\n">>),
      From ! read_address(Port);
    {From, command, Command} ->
      port_command(Port, <<Command/binary, "\r\n">>),
      From ! read_details(Port);
    {Port, {data, {eol, Data}}} ->
      ?DEBUG(">> ~p", [Data]);
    stop ->
      port_command(Port, <<"exit\r\n">>),
      Port ! {self(), close};
    {'EXIT', Port, _Reason} ->
      ?INFO("Port exited..."),
      exit(port_terminated)
  after
    60*1000 ->
      port_command(Port, <<"exit\r\n">>),
      Port ! {self(), close}
  end,
  loop(Port).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
read_balance(Port) ->
  receive
    {Port, {data, {eol, Data}}} ->
      ?DEBUG("Wallet balance: ~p", [Data]),
      [_,_,_,_, Bal, _,_, Locked] = binary:split(Data, <<" ">>, [global]),
      {ok, {balance, Bal}, {locked, Locked}}
  end.

read_address(Port) ->
  receive
    {Port, {data, {eol, Data}}} ->
      [_,_, Addr] = binary:split(Data, <<" ">>, [global]),
      {ok, {address, Addr}}
  end.

read_details(Port) ->
  receive
    {Port, {data, {eol, Data}}} ->
      A = binary:split(Data, <<"\r\n">>, [global]),
      {ok, A}
  end.
