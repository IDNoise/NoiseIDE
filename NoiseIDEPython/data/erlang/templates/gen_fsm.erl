%% @author: [username]
%% @date: [date]

-module([module_name]).

-behaviour(gen_fsm).

%% API
-export([
	start_link/0
]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() -> gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
    {ok, state_name, #state{}}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) -> ok.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================





