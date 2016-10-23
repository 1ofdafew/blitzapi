-module (blitzapi_wallet_handler).
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
    File = <<Dir/binary, "/", Wallet/binary>>,
    case file:list_dir(binary_to_list(File)) of
      {error, enoent} ->
        % ok, we can create the wallet
        Pid = blitzapi_port:start(new, {Wallet, Password}),
        timer:sleep(1000),
        {ok, Address} = blitzapi_port:cmd(Pid, <<"address">>),
        blitzapi_port:stop(Pid),

        % return result
        Reply = [Address, {server_time, iso8601:format(Now)}],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        {true, Req2, State};
      {error, enotdir} ->
        Reply = [{error, <<"Wallet exists">>},
                 {server_time, iso8601:format(Now)}],
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
        {false, Req2, State}
    end
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
