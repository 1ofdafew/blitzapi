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
-module(blitzapi_port).
-author ("DaFew <1ofdafew@gmail.com>").
-vsn("1.0.1").

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
      ?INFO("Running cmd: ~p", [Command]),
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
      try
        [_,_,_,_, Bal, _,_, Locked] = binary:split(Data, <<" ">>, [global]),
        {ok, {balance, Bal}, {locked, Locked}}
      catch
        _:_ ->
          {error, bad_wallet_creds}
      end
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
