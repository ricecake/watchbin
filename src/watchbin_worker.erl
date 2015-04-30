-module(watchbin_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

-export([start_timer/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(watchbin, {width, callback, container=#{}}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(BucketSize, Callback) ->
    gen_server:start_link(?MODULE, #watchbin{width=BucketSize, callback=Callback}, []).

start_timer(Worker, Interval, Data, Opts) ->
	gen_server:call(Worker, {add, Interval, Data, Opts}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({add, Interval, Data, Opts}, _From, #watchbin{container=Map, width=BucketSize} = State) ->
	Jitter = proplists:get_bool(jitter, Opts),
	WaitTime = if
		Jitter -> random:uniform(Interval);
		not Jitter -> Interval
	end,
	NewMap = add(Map, BucketSize, WaitTime, {Interval, Data}),
	{reply, ok, State#watchbin{container=NewMap}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tick, Timeout}, #watchbin{container=Map, width=BucketSize, callback=CallBack} = State) ->
	NewMap = lists:foldl(fun({Int, Data} = Value, OldMap) ->
		CallBack(Data),
		add(OldMap, BucketSize, Int, Value)
	end, maps:remove(Timeout, Map), maps:get(Timeout, Map)),
	{noreply, State#watchbin{container=NewMap}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

add(Map, BucketSize, Delay, Data) ->
	Now = timestamp(),
	Timeout = max(round(Now+BucketSize), BucketSize*round((Now+Delay)/BucketSize)),
	NewValue = case maps:find(Timeout, Map) of
		{ok, List} when is_list(List) -> [Data| List];
		error                         ->
					erlang:send_after(Delay, self(), {tick, Timeout}),
					[Data]
	end,
	maps:put(Timeout, NewValue, Map).

timestamp() -> 
	{Mega, Secs, Micro} = os:timestamp(),
	Mega*1000*1000*1000 + Secs * 1000 + (Micro / 1000).

