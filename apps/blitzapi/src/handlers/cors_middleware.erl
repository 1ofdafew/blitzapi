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
-module (cors_middleware).
-author ("DaFew <1ofdafew@gmail.com>").
-behaviour (cowboy_middleware).
-vsn("1.0.1").

-include ("blitzapi.hrl").

-export ([execute/2]).

execute(Req, State) ->
  Req2 = set_cors_headers(Req),
  {Method, Req3} = cowboy_req:method(Req2),
  case Method of
    <<"OPTIONS">> ->
      {ok, Req4} = cowboy_req:reply(200, Req3),
      {halt, Req4};
    _ ->
      {ok, Req3, State}
  end.

set_cors_headers(Req) ->
  Headers = [
    {<<"access-control-allow-origin">>, <<$*>>},
    {<<"access-control-allow-headers">>, <<"origin, x-requested-with, content-type, accept, authorization">>}
  ],
  add_headers(Req, Headers).

add_headers(Req, Headers) ->
  F = fun({Tag, Val}, Req1) ->
        Req2 = cowboy_req:set_resp_header(Tag, Val, Req1),
        Req2
      end,
  lists:foldl(F, Req, Headers).
