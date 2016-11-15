-module(watchbin).

-export([
	start/0,
	new/2,
	new/3,
	destroy/1,
	start_timer/4,
	stop_timer/2
]).

start() -> application:ensure_all_started(watchbin).

new(BucketSize, Callback) ->
	watchbin_srv:new(BucketSize, Callback).

new(BucketSize, Callback, Label) ->
	watchbin_srv:new(BucketSize, Callback, Label).

destroy(Pid) ->
	watchbin_srv:destroy(Pid).

start_timer(Pid, Interval, Data, Opts) when is_integer(Interval), is_list(Opts) ->
	watchbin_worker:start_timer(Pid, Interval, Data, Opts).

stop_timer(Pid, TimerID) ->
	watchbin_worker:stop_timer(Pid, TimerID).
