-module(watchbin_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([create/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create(BucketSize, Callback, Label) -> supervisor:start_child(?MODULE, [BucketSize, Callback, Label]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	ets:new(watchbin_worker_registry, [
		bag,
		public,
		named_table,
		{read_concurrency, true}
	]),

	{ok, { {simple_one_for_one, 5, 10}, [
		?CHILD(watchbin_worker, worker)
	]}}.
