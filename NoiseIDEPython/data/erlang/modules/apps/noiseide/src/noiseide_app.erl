%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 07.02.2013

-module(noiseide_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start() ->
    application:start(noiseide).

-spec start(StartType, StartArgs) -> Result when
      		StartType :: normal | {takeover, node()} | {failover, node()},
      		StartArgs :: term(),
            Result :: {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(noiseide, port),
    {ok, eide_connect:start(Port)}.

-spec stop(State :: term()) -> term().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================




