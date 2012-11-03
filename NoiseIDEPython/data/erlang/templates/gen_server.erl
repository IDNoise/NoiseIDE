%% @author: [username]
%% @date: [date]

-module([module_name]).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid} | ignore | {error, Error} when
    Pid :: pid(),
    Error :: {already_started, Pid} | term().

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args) -> Result when
    Args :: term(),
    Result :: {ok, State}
     | {ok, State, timeout() | hibernate}
     | {stop, Reason}
     | ignore,
    State :: term(),
    Reason :: term().

init([]) ->
    {ok, #state{}}.

-spec handle_call(Request, From, State) -> Result when
      Request :: term(),
      From :: {pid(), Tag :: term()},
      State :: term(),
      Result :: {reply, Reply, NewState}
        | {reply, Reply, NewState, timeout() | hibernate}
        | {noreply, NewState}
        | {noreply, NewState, timeout() | hibernate}
        | {stop, Reason, Reply, NewState}
        | {stop, Reason, NewState},
       Reply :: term(),
       NewState :: term(),
       Reason :: term().

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Request, State) -> Result when
      Request :: term(),
      State :: term(),
      Result :: {noreply, NewState}
        | {noreply, NewState, timeout() | hibernate}
        | {stop, Reason, NewState},
       NewState :: term(),
       Reason :: term().

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> Result when
      Info :: timeout | term(),
      State :: term(),
      Result :: {noreply, NewState}
        | {noreply, NewState, timeout() | hibernate}
        | {stop, Reason, NewState},
       NewState :: term(),
       Reason :: normal | term().

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> any() when
      Reason :: normal | shutdown | {shutdown, term()} | term(),
      State :: term().

terminate(_Reason, _State) -> ok.

-spec code_change(OldVsn, State:: term(), Extra :: term()) -> Result when
      OldVsn :: Vsn :: term() | {down, Vsn :: term()},
      Result :: {ok, NewState :: term()} | {error, Reason :: term()}.

code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




