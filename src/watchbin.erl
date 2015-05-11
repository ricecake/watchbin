-module(watchbin).

-export([
	start/0,
	new/2,
	destroy/1,
	start_timer/4,
	stop_timer/2
]).

start() -> application:ensure_all_started(watchbin).

new(BucketSize, Callback) ->
	{ok, Pid} = watchbin_srv:new(BucketSize, Callback),
	{ok, {watchbin, Pid}}.

destroy({watchbin, Pid}) ->
	watchbin_srv:destroy(Pid).

start_timer({watchbin, Pid}, Interval, Data, Opts) when is_integer(Interval), is_list(Opts) ->
	watchbin_worker:start_timer(Pid, Interval, Data, Opts).

stop_timer({watchbin, Pid}, TimerID) ->
	watchbin_worker:stop_timer(Pid, TimerID).

