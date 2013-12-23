%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 22.12.2013

-module(noiseide_api).

%% Include files

%% Exported Functions

-export([
    go_to/1,
    go_to/2,
    go_to_file/1,
    go_to_file/2
]).

%%%===================================================================
%%% API
%%%===================================================================

go_to(Module) -> 
    go_to(Module, 0).

go_to(Module, Line) ->
    eide_client_connect:send(go_to, [{module, Module}, {line, Line}]).
    
go_to_file(File) -> 
    go_to_file(File, 0).

go_to_file(File, Line) ->
    eide_client_connect:send(go_to_file, [{file, list_to_binary(File)}, {line, Line}]).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================





