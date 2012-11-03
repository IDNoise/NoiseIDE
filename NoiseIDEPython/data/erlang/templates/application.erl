%% @author: [username]
%% @date: [date]

-module([module_name]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(StartType, StartArgs) -> Result when
      		StartType :: normal | {takeover, node()} | {failover, node()},
      		StartArgs :: term(),
            Result :: {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.

start(_StartType, _StartArgs) ->
    case 'TopSupervisor':start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

-spec stop(State :: term()) -> term().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================




