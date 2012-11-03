%% @author: [username]
%% @date: [date]

-module([module_name]).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
                 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

-spec start_link() -> gen_event:start_ret().

start_link() ->
    gen_event:start_link({local, ?SERVER}).

-spec add_handler() -> term().

add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

-spec init(InitArgs) -> Result when
    InitArgs :: term(),
    Result :: {ok, State} | {ok, State, hibernate} | {error, Reason},
    State :: term(),
    Reason :: term().

init([]) ->
    {ok, #state{}}.

-spec handle_event(Event :: term(), State :: term()) -> Result when
    Result :: {ok, NewState} | {ok, NewState, hibernate}
    | {swap_handler, Args1 :: term(), NewState, Handler2, Args2 :: term()}
    | remove_handler,
    Handler2 :: atom() | {atom(), Id :: term()},
    NewState :: term().

handle_event(_Event, State) ->
    {ok, State}.

-spec handle_call(Request :: term(), State :: term()) -> Result when
    Result :: {ok, Reply, NewState}
    | {ok, Reply, NewState, hibernate}
    | {swap_handler, Reply, Args1 :: term(), NewState, Handler2, Args2 :: term()}
    | {remove_handler, Reply},
    NewState :: term(),
    Reply :: term(),
    Handler2 :: atom() | {atom(), Id :: term()}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

-spec handle_info(Info :: term(), State :: term()) -> Result when
    Result :: {ok, NewState}
    | {ok, NewState, hibernate}
    | {swap_handler, Args1 :: term(), NewState, Handler2, Args2 :: term()}
    | remove_handler,
    NewState :: term(),
    Handler2 :: atom() | {atom(), Id :: term()}.

handle_info(_Info, State) ->
    {ok, State}.

-spec terminate(Args, State :: term()) -> term() when
    Args :: term()
    | {stop, Reason :: term()}
    | stop
    | remove_handler
    | {error, {'EXIT', Reason :: term()}}
    | {error, term()}.

terminate(_Reason, _State) -> ok.

-spec code_change(OldVsn, State :: term(), Extra :: term()) ->
        {ok, NewState :: term()} when
    OldVsn :: term() | {down, term()}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



