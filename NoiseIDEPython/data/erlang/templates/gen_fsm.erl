%% @author: [username]
%% @date: [date]

-module([module_name]).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid} | ignore | {error, Error} when
    Pid :: pid(),
    Error :: {already_started, Pid} | term().

start_link() -> gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec init(Args :: term()) -> Result when
    Result :: {ok, StateName, StateData}
    | {ok, StateName, StateData, timeout() | hibernate}
    | {stop, Reason :: term()}
    | ignore,
    StateName :: atom(),
    StateData :: term().

init([]) ->
    {ok, state_name, #state{}}.

-spec handle_event(Event :: term(), StateName :: atom(), StateData :: term()) -> Result when
    Result :: {next_state, NextStateName, NewStateData}
    | {next_state, NextStateName, NewStateData, timeout() | hibernate}
    | {stop, Reason :: term(), NewStateData},
    NextStateName :: atom(),
    NewStateData :: term().

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

-spec handle_sync_event(Event :: term(), From, StateName, StateData) -> Result when
    From :: {pid(), Tag :: term()},
    StateName :: atom(),
    StateData :: term(),
    Result :: {reply, Reply, NextStateName, NewStateData}
    | {reply, Reply, NextStateName, NewStateData, timeout() | hibernate}
    | {next_state, NextStateName, NewStateData}
    | {next_state, NextStateName, NewStateData, timeout() | hibernate}
    | {stop, Reason, Reply, NewStateData}
    | {stop, Reason, NewStateData},
    NextStateName :: atom(),
    NewStateData :: term(),
    Reply :: term(),
    Reason :: term().

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

-spec handle_info(Info :: term(), StateName, StateData) -> Result when
    StateName :: atom(),
    StateData :: term(),
    Result :: {next_state, NextStateName, NewStateData}
    | {next_state, NextStateName, NewStateData, timeout() | hibernate}
    | {stop, Reason :: normal | term(), NewStateData},
    NextStateName :: atom(),
    NewStateData :: term().

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

-spec terminate(Reason, StateName, StateData) -> term() when
    StateName :: atom(),
    StateData :: term(),
    Reason :: normal | shutdown | {shutdown, term()} | term().

terminate(_Reason, _StateName, _State) -> ok.

-spec code_change(OldVsn, StateName, StateData, Extra :: term()) -> Result when
    OldVsn :: term() | {down, term()},
    StateName :: atom(),
    StateData :: term(),
    Result :: {ok, NextStateName :: atom(), NewStateData :: term()}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================





