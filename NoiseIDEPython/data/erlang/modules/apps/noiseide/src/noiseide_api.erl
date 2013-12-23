%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 22.12.2013

-module(noiseide_api).

%% Include files

%% Exported Functions

-export([
    goto_line/1,
    goto_line/2,
    goto_file/1,
    goto_file/2,
    goto_mfa/3
]).

%%%===================================================================
%%% API
%%%===================================================================

goto_line(Module) -> 
    goto_line(Module, 0).

goto_line(Module, Line) ->
    eide_client_connect:send(goto_line, [{module, Module}, {line, Line}]).
    
goto_file(File) -> 
    goto_file(File, 0).

goto_file(File, Line) ->
    eide_client_connect:send(goto_file, [{file, list_to_binary(File)}, {line, Line}]).
 
goto_mfa(Module, Fun, Arity) ->
    eide_client_connect:send(goto_mfa, [{module, Module}, {'fun', Fun}, {arity, Arity}]).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================





