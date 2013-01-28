%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 09.09.2012

-module(test_cache_module1).

%% Include files
-include_lib("noiseide/include/sample.hrl").
%% Exported Functions

-export([
    x/0,
    xxx/0,
    round/2
]).    
-define(XssssX, ololasdasdado).  
-type beam_instr() :: 'bs_init_writable' | 'fclearerror' | 'if_end'
                    | 'remove_message' | 'return' | 'send' | 'timeout'
                    | tuple().  %% XXX: Very underspecified - FIX THIS 
   
 
%%%========================== ==== ======= ==============================
%%% API  

%-include("sample.hrl
%%%============================  ======= ================================
      
-record(ololo, {       
    field_1 :: beam_instr(),  
    field_2 :: atom() | boolean() | ololoxxx,
    field_3 :: dialyzer_plt:plt_info()
}).       
      
%asdasdasd  \
%asdasdasd
%asdasdasd
      
-opaque ololo() :: #ololo{}.
%%%===================================================================
%%% Internal f unction s
%%%====================================== =============================

%% @doc Prints the value X.
xxx() ->
    ?XX + ?XssssX.  

-ifdef(debug).
-define(X, xx).
-else.
-define(X, yy).
-endif.  
  
x() -> 
    xxx(),
    SortedQueueDiffs = 
        lists:sort(fun({{_, IR1}, D1}, {{_, IR2}, D2}) -> 
            Add1 = case IR1 of true -> 1; _ -> 0 end, 
            Add2 = case IR2 of true -> 1; _ -> 0 end, 
            D1 * 1000 + Add1 < D2 * 1000 + Add2 
        end, []),
    io:format("~p~n", [?X]).
    
-spec round(XX, integer()) -> integer()
  when XX :: atom().
%% @doc Prints the value X.
%round(Price, buy) ->
%    [ {Key, apmath:ceil(Count)} || {Key, Count} <- Price ];
round(Price, sell) ->
    [ {Key, trunc(Count)} || {Key, Count} <- Price ].

