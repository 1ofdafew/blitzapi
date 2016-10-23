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

-define(DEBUG(Text), lager:log(debug, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(DEBUG(Text, Args), lager:log(debug, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(INFO(Text), lager:log(info, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(INFO(Text, Args), lager:log(info, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(WARN(Text), lager:log(warning, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(WARN(Text, Args), lager:log(warning, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(ERROR(Text), lager:log(error, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(ERROR(Text, Args), lager:log(error, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-record(blitzapi_users, {username, password, created, updated, active}).
-record(blitzapi_tokens, {token, created, valid_until}).
