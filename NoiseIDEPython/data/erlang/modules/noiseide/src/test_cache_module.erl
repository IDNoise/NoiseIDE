%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 09.09.2012

-module(test_cache_module).

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
    io:format("~p~n", [?X]).
    
-spec round(XX, integer()) -> integer()
  when XX :: atom().
%% @doc Prints the value X.
round(Price, buy) ->
    [ {Key, apmath:ceil(Count)} || {Key, Count} <- Price ];
round(Price, sell) ->
    [ {Key, trunc(Count)} || {Key, Count} <- Price ].

