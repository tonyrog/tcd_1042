%%% coding: latin-1
%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Marina Westman Lönne <malotte@malotte.net>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Marinco search light controller
%%% @end
%%% Created : 1 Nov 2015 by Tony Rogvall <tony@rogvall.se>

-module(tcd_1042).

-export([start/0]).
%% button api
-export([press/1, release/1, getkeys/0]).
-export([subscribe/0, unsubscribe/1]).
-export([get_status/1]).

%% test api
-export([pause/0, resume/0]).

-define(SERVER, tcd_1042_srv).

start() ->
    application:start(lager),
    application:start(uart),
    application:start(tcd_1042).

press(Key) when is_atom(Key); is_list(Key) ->
    ?SERVER:press(Key).

release(Key) when is_atom(Key); is_list(Key) ->
    ?SERVER:release(Key).

getkeys() ->
    ?SERVER:getkeys().

get_status(Key) ->
    ?SERVER:get_status(Key).

subscribe() ->
    ?SERVER:subscribe().

unsubscribe(Ref) ->
    ?SERVER:unsubscribe(Ref).

pause() ->
    ?SERVER:pause().

resume() ->
    ?SERVER:resume().
