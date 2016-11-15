-module(watchbin_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).

-export([start_timer/4, stop_timer/2, destroy/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(watchbin, {width, callback, label, container=#{}, counter=0, data=#{}}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(BucketSize, Callback, Label) ->
    gen_server:start_link(?MODULE, #watchbin{width=BucketSize, callback=Callback, label=Label}, []).

start_timer(Worker, Interval, Data, Opts) ->
	[{Worker, Pid}] = ets:lookup(watchbin_worker_registry, Worker),
	gen_server:call(Pid, {add, Interval, Data, Opts}).

stop_timer(Worker, TimerId) ->
	[{Worker, Pid}] = ets:lookup(watchbin_worker_registry, Worker),
	gen_server:call(Pid, {remove, TimerId}).

destroy(Worker) ->
	[{Worker, Pid}] = ets:lookup(watchbin_worker_registry, Worker),
	gen_server:call(Pid, destroy).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(#watchbin{label=Label} = Args) ->
	ets:insert(watchbin_worker_registry, {Label, self()}),
	{ok, Args}.

handle_call(destroy, _From, State) ->
	{stop, normal, ok, State};
handle_call({remove, TimerId}, _From, State) ->
	{reply, ok, State#watchbin{data=maps:remove(TimerId, State#watchbin.data)}};
handle_call({add, Interval, Data, Opts}, _From, #watchbin{container=Map, width=BucketSize, counter=ID, data=Jobs} = State) ->
	Jitter = proplists:get_bool(jitter, Opts),
	WaitTime = if
		Jitter -> random:uniform(Interval);
		not Jitter -> Interval
	end,
	JobId = proplists:get_value(name, Opts, ID),
	NewJobs = maps:put(JobId, {Interval, Data, Opts}, Jobs),
	NewMap = add(Map, BucketSize, WaitTime, JobId),
	{reply, {ok, {watchbin_tref, JobId}}, State#watchbin{container=NewMap, counter=ID+1, data=NewJobs}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tick, Timeout}, #watchbin{container=Map, width=BucketSize, callback=CallBack, data=Jobs} = State) ->
	NewMap = lists:foldl(fun(ID, OldMap) ->
		case maps:find(ID, Jobs) of
			{ok, Details} -> process(CallBack, Details, OldMap, BucketSize, ID);
			error         -> OldMap
		end
	end, maps:remove(Timeout, Map), maps:get(Timeout, Map)),
	{noreply, State#watchbin{container=NewMap}}.

terminate(_Reason, State) ->
	ets:delete(watchbin_worker_registry, State#watchbin.label),
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

process(Callback, {Interval, Data, Options}, Map, Width, Id) ->
	OneTime = proplists:get_bool(once, Options),
	if
		OneTime ->
			Callback(Data),
			Map;
		not OneTime ->
			Callback(Data),
			add(Map, Width, Interval, Id)
	end.

timestamp() ->
	{Mega, Secs, Micro} = os:timestamp(),
	Mega*1000*1000*1000 + Secs * 1000 + (Micro / 1000).
