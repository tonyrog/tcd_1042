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
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Marina Westman Lönne <malotte@malotte.net>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Marinco search light controller
%%% @end
%%% Created : 1 Nov 2015 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(tcd_1042_srv).

-behaviour(gen_server).

%% API
-export([start_link/0,start_link/1]).
-export([config_change/3]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% button api
-export([press/1, release/1, getkeys/0]).
-export([subscribe/0, unsubscribe/1]).
-export([value/1]).

%% test api
-export([pause/0, resume/0, ifstatus/0]).
-export([dump/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_RETRY_INTERVAL, 2000).
-define(DEFAULT_BAUDRATE, 9600).

%% avoid sending 3 and 12
-define(BOW_ONOFF,     2#00000001).   %% 1
-define(BOW_PORT,      2#00000010).   %% 2
-define(BOW_STARB,     2#00000100).   %% 4
-define(STERN_ONOFF,   2#00001000).   %% 16
-define(STERN_PORT,    2#00010000).   %% 32
-define(STERN_STARB,   2#00100000).   %% 64

-define(STATUS_OFF,     0).
-define(STATUS_ON,      1).
-define(STATUS_STANDBY, 2).

-record(sub,
	{
	  pid,
	  ref
	}).

-record(s, {
	  uart,             %% serial line port id
	  device,           %% device name | simulated | none
	  baud_rate,        %% baud rate to uart
	  retry_interval,   %% Timeout for open retry
	  retry_timer,      %% Timer reference for retry
	  pause = false,    %% Pause input
	  key_mask = 0,     %% Keys that are pressed
	  stern_status = 0, %% stern thruster is off (on = 1)
	  bow_status = 0,   %% bow thruster is off (on = 1)
	  buf = <<>>,       %% binary buffer
	  subs = []         %% subscription list
	 }).

-define(ite(C,T,E), if (C) -> (T); true -> (E) end).
			     
%%%===================================================================
%%% API
%%%===================================================================

press(Key) when is_atom(Key); is_list(Key) ->
    Mask = keymask(Key),
    gen_server:cast(?SERVER, {press,Mask}).

release(Key) when is_atom(Key); is_list(Key) ->
    Mask = keymask(Key),
    gen_server:cast(?SERVER, {release,Mask}).

%% get pressed keys
getkeys() ->
    case gen_server:call(?SERVER, getkeys) of
	{ok,Mask} ->
	    {ok,?ite(Mask band ?BOW_STARB =/= 0,[bow_thruster_starboard],[]) ++
		 ?ite(Mask band ?BOW_PORT =/= 0,[bow_thruster_port],[]) ++
		 ?ite(Mask band ?BOW_ONOFF =/= 0,[bow_onoff],[]) ++
		 ?ite(Mask band ?STERN_STARB =/= 0,[sternthruster_starboard],[]) ++
		 ?ite(Mask band ?STERN_PORT =/= 0,[stern_thruster_port],[]) ++
		 ?ite(Mask band ?STERN_ONOFF =/= 0,[stern_onoff],[])};
	Error ->
	    Error
    end.

-spec value(Key::atom()) -> Value::string() | {error,Reason::term()}.
value(Key) ->
    gen_server:call(?SERVER, {value, Key}).

-spec subscribe() -> {ok,reference()} | {error,Reason::term()}.
subscribe() ->
    gen_server:call(?SERVER, {subscribe, self()}).

-spec unsubscribe(Ref::reference()) -> ok | {error,enoent}.
unsubscribe(Ref) when is_reference(Ref) ->
    gen_server:call(?SERVER, {unsubscribe, Ref}).

keymask(Key) ->
    case Key of
	bow_thruster_starboard   -> ?BOW_STARB;   %% green
	bow_thruster_port        -> ?BOW_PORT;    %% red
	bow_thruster_onoff       -> ?BOW_ONOFF;   %%
	stern_thruster_starboard -> ?STERN_STARB; %% green
	stern_thruster_port      -> ?STERN_PORT;  %% red
	stern_thruster_onoff     -> ?BOW_ONOFF;   %%
	[K|Ks] -> keymask(K) bor keymask(Ks);
	[] -> 0
    end.

-spec pause() -> ok | {error, Error::atom()}.
pause() ->
    gen_server:call(?SERVER, pause).

-spec resume() -> ok | {error, Error::atom()}.
resume() ->
    gen_server:call(?SERVER, resume).

-spec ifstatus() -> ok | {error, Error::atom()}.
ifstatus() ->
    gen_server:call(?SERVER, ifstatus).

-spec dump() -> ok | {error, Error::atom()}.
dump() ->
    gen_server:call(?SERVER, dump).

config_change(Changed,New,Removed) ->
    gen_server:call(?SERVER, {config_change,Changed,New,Removed}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Opts0) ->
    lager:debug("opts ~p",[Opts0]),
    Opts = Opts0 ++ application:get_all_env(tcd_1042),
    RetryInterval = proplists:get_value(retry_interval,Opts,
					?DEFAULT_RETRY_INTERVAL),
    Pause = proplists:get_value(pause, Opts, false),
    Device = case proplists:get_value(device, Opts) of
		 undefined -> os:getenv("TCD_1042_DEVICE");
		 D -> D
	     end,
    Baud = case proplists:get_value(baud, Opts) of
	       undefined ->
		   case os:getenv("TCD_1042_SPEED") of
		       false -> ?DEFAULT_BAUDRATE;
		       ""    -> ?DEFAULT_BAUDRATE;
		       Baud0 -> list_to_integer(Baud0)
		   end;
	       Baud1 -> Baud1
	   end,
    S = #s{ device = Device,
	    baud_rate = Baud,
	    retry_interval = RetryInterval,
	    pause = Pause
	  },
    if Device =:= false; Device =:= "" ->
	    lager:error("tcd_1042: missing device argument"),
	    {stop, einval};
       true ->
	    lager:info("tcd_1042: using device ~s@~w\n", 
		  [Device, Baud]),
	    case open(S) of
		{ok, S1} -> {ok, S1};
		Error -> {stop, Error}
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({press,Mask}, _From, S) ->
    KeyMask = S#s.key_mask bor Mask,
    Reply = send_mask(KeyMask, S),
    {reply, Reply, S#s { key_mask = KeyMask }};
handle_call({release,Mask}, _From, S) ->
    KeyMask = S#s.key_mask band (bnot Mask),
    Reply = send_mask(KeyMask, S),
    {reply, Reply, S#s { key_mask = KeyMask }};
handle_call(getkeys, _From, S) ->
    {reply, {ok,S#s.key_mask}, S};
handle_call({value, Key}, _From, S) ->
    {reply, value(Key, S), S};
handle_call({subscribe,Pid}, _From, S) ->
    Ref = erlang:monitor(process, Pid),
    Sub = #sub { pid=Pid, ref=Ref },
    {reply, {ok,Ref}, S#s { subs = [Sub|S#s.subs] }};
handle_call({unsubscribe,Ref}, _From, S) ->
    case lists:keytake(Ref, #sub.ref, S#s.subs) of
	false ->
	    {reply, {error, enoent}, S};
	{value,Sub,Subs} ->
	    demonitor(Sub#sub.ref),
	    {reply, ok, S#s { subs = Subs }}
    end;
handle_call(pause, _From, S=#s {pause = false, uart = Uart}) 
  when Uart =/= undefined ->
    lager:debug("pause.", []),
    lager:debug("closing device ~s", [S#s.device]),
    R = uart:close(S#s.uart),
    lager:debug("closed ~p", [R]),
    {reply, ok, S#s {pause = true}};
handle_call(pause, _From, S) ->
    lager:debug("pause when not active.", []),
    {reply, ok, S#s {pause = true}};
handle_call(resume, _From, S=#s {pause = true}) ->
    lager:debug("resume.", []),
    case open(S#s {pause = false}) of
	{ok, S1} -> {reply, ok, S1};
	Error -> {reply, Error, S}
    end;
handle_call(resume, _From, S=#s {pause = false}) ->
    lager:debug("resume when not paused.", []),
    {reply, ok, S};
handle_call(ifstatus, _From, S=#s {pause = Pause}) ->
    lager:debug("ifstatus.", []),
    {reply, {ok, if Pause -> paused; true -> active end}, S};
handle_call(dump, _From, S) ->
    lager:debug("dump.", []),
    {reply, {ok, S}, S};
handle_call({config_change,_Changed,_New,_Removed},_From,S) ->
    lager:debug("config_change changed=~p, new=~p, removed=~p\n",
		[_Changed,_New,_Removed]),
    {reply, ok, S};

handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({press,Mask}, S) ->
    KeyMask =  S#s.key_mask bor Mask,
    _Reply = send_mask(KeyMask, S),
    {noreply, S#s { key_mask = KeyMask }};
handle_cast({release,Mask}, S) ->
    KeyMask =  S#s.key_mask band (bnot Mask),
    _Reply = send_mask(KeyMask, S),
    {noreply, S#s { key_mask = KeyMask }};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({uart,U,Data}, S) when S#s.uart =:= U ->
    S1 = handle_input(<<(S#s.buf)/binary, Data/binary>>, S),
    uart:setopt(U, active, once),
    {noreply, S1};

handle_info({uart_error,U,Reason}, S) when U =:= S#s.uart ->
    if Reason =:= enxio ->
	    lager:error("tcd_1042: uart error ~p device ~s unplugged?", 
			[Reason,S#s.device]),
	    {noreply, reopen(S)};
       true ->
	    lager:error("tcd_1042: uart error ~p for device ~s", 
			[Reason,S#s.device]),
	    {noreply, S}
    end;

handle_info({uart_closed,U}, S) when U =:= S#s.uart ->
    lager:error("tcd_1042: uart device closed, will try again in ~p msecs.",
		[S#s.retry_interval]),
    S1 = reopen(S),
    {noreply, S1};

handle_info({timeout,Timer,reopen},S) when Timer =:= S#s.retry_timer ->
    case open(S#s { retry_timer = undefined }) of
	{ok, S1} ->
	    {noreply, S1};
	Error ->
	    {stop, Error, S}
    end;

handle_info({timeout,_Timer,simulated_status},S) ->
    Bow = random:uniform(3)-1,
    Stern = random:uniform(3)-1,
    S1 = handle_status(<<0:4,Stern:2,Bow:2>>, S),
    start_timer(5000, simulated_status),
    {noreply, S1};

handle_info({'DOWN',Ref,process,Pid,Reason}, S) ->
    lager:debug("tcd_1042: process ~p crashed, reason ~p", [Pid,Reason]),
    case lists:keytake(Ref, #sub.ref, S#s.subs) of
	false ->
	    {noreply, S};
	{value,_Sub,Subs} ->
	    {noreply, S#s { subs = Subs }}
    end;
handle_info(_Info, S) ->
    lager:debug("tcd_1042: got info ~p", [_Info]),
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_mask(Mask, #s { device = simulated }) ->
    lager:info("sending ~w.",[Mask]);
send_mask(_Mask, #s { device = none }) ->
    ok;
send_mask(Mask, #s { uart = U }) when U =/= undefined ->
    lager:debug("sending ~w.",[Mask]),
    uart:send(U, [Mask]);
send_mask(_Mask, _S) ->
    ok.

handle_input(Buffer, S) ->
    lager:debug("handle_input: got ~p", [Buffer]),
    S1 = handle_status(Buffer, S),
    S1#s { buf = <<>> }.

handle_status(Buffer, S) ->
    case Buffer of
	<<_:4, Stern:2, Bow:2, Rest/binary>> ->
	    S1 = if Stern =/= S#s.stern_status ->
			 send_status(S#s { stern_status = Stern });
		    true ->
			 S
		 end,
	    S2 = if Bow =/= S1#s.bow_status ->
			 send_status(S1#s { bow_status = Bow });
		    true ->
			 S1
		 end,
	    handle_status(Rest, S2);
	<<>> ->
	    S
    end.

%% send status to subscribers, return state!
send_status(S) when S#s.subs =:= [] ->
    S;
send_status(S) ->
    Message =
	[{bow_thruster,
	  case S#s.bow_status of
	      ?STATUS_OFF     -> off;
	      ?STATUS_ON      -> on;
	      ?STATUS_STANDBY -> standby;
	      _ -> unknown
	  end},
	 {stern_thruster,
	  case S#s.stern_status of
	      ?STATUS_OFF      -> off;
	      ?STATUS_ON       -> on;
	      ?STATUS_STANDBY  -> standby;
	      _ -> unknown
	  end}
	],
    lists:foreach(
      fun(Sub) ->
	      Sub#sub.pid ! {tcd_1042, Sub#sub.ref, Message}
      end, S#s.subs),
    S.

value(bow_thruster_status, S) ->
    integer_to_list(S#s.bow_status);
value(stern_thruster_status, S) ->
    integer_to_list(S#s.stern_status);
value(Key,S) ->
    Pressed = (keymask(Key) band S#s.key_mask) =/= 0,
    ?ite(Pressed,"1","0").
	    

open(S0=#s {device = none }) ->
    {ok, S0};
open(S0=#s {pause = true}) ->
    {ok, S0};
open(S0=#s {device = simulated }) ->
    lager:debug("tcd_1042: simulated"),
    start_timer(5000, simulated_status),
    {ok, S0};
open(S0=#s {device = DeviceName, baud_rate = Baud }) ->
    UartOpts = [{mode,binary}, {baud, Baud}, {packet, 0},
		{csize, 8}, {stopb,1}, {parity,none}, {active, once}],
    case uart:open(DeviceName, UartOpts) of
	{ok,Uart} ->
	    lager:debug("tcd_1042:open: ~s@~w", [DeviceName,Baud]),
	    {ok, S0#s { uart = Uart }};
	{error,E} when E =:= eaccess; E =:= enoent ->
	    lager:debug("tcd_1042:open: ~s@~w  error ~w, will try again "
		   "in ~p msecs.", [DeviceName,Baud,E,S0#s.retry_interval]),
	    {ok, reopen(S0)};
	Error ->
	    lager:error("tcd_1042: error ~w", [Error]),
	    Error
    end.

reopen(S=#s {pause = true}) ->
    S;
reopen(S) ->
    if S#s.uart =/= undefined ->
	    lager:debug("tcd_1042: closing device ~s", [S#s.device]),
	    R = uart:close(S#s.uart),
	    lager:debug("tcd_1042: closed ~p", [R]),
	    R;
       true ->
	    ok
    end,
    Timer = start_timer(S#s.retry_interval, reopen),
    S#s { uart=undefined, buf=(<<>>), retry_timer=Timer }.

start_timer(undefined, _Tag) ->
    undefined;
start_timer(infinity, _Tag) ->
    undefined;
start_timer(Time, Tag) ->
    erlang:start_timer(Time,self(),Tag).
