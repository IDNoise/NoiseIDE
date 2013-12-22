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
    Result = case application:get_env(noiseide, port) of
        undefined -> ignore;
        {ok, Port} -> {ok, eide_connect:start(Port)}
    end,
    case application:get_env(noiseide, client_port) of
        undefined -> Result;
        {ok, ClientPort} -> {ok, eide_client_connect:start(ClientPort)}
    end.


-spec stop(State :: term()) -> term().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================




