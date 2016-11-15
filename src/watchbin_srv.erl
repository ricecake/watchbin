-module(watchbin_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([new/2, new/3, destroy/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new(BucketSize, Callback) -> new(BucketSize, Callback, make_ref()).

new(BucketSize, Callback, Label) when is_integer(BucketSize), is_function(Callback) ->
	gen_server:call(?SERVER, {create, BucketSize, Callback, Label}).

destroy(Pid) ->
	gen_server:call(?SERVER, {destroy, Pid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({destroy, Pid}, _From, State) ->
	ok = watchbin_worker:destroy(Pid),
	{reply, ok, State};
handle_call({create, BucketSize, Callback, Label}, _From, State) ->
	NewBin = watchbin_worker_sup:create(BucketSize, Callback, Label),
	{reply, NewBin, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
