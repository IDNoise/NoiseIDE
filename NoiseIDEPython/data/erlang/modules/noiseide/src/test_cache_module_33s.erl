%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 09.09.2012

-module(test_cache_module_33s).

%% Include files
-include("sample.hrl").
%% Exported Functions

-export([
     x/0,
    xxx/0
]).    
-define(XssssX, ololasdasdado).   
-type beam_instr() :: 'bs_init_writable' | 'fclearerror' | 'if_end'
                    | 'remove_message' | 'return' | 'send' | 'timeout'
                    | tuple().  %% XXX: Very underspecified - FIX THIS 
   
 
%%%========================== ==== ======= ==============================
%%% API  
%%%============================  ======= ================================
     
-record(ololo, {     
    field_1 :: beam_instr(),  
    field_2 :: atom() | boolean() | ololoxxx,
    field_3 :: dialyzer_plt:plt_info()
}).       
      
-opaque ololo() :: #ololo{}. 
%%%===================================================================
%%% Internal f unction s
%%%====================================== =============================
xxx() ->
    ?XX + ?XssssX.   

-ifdef(debug).
-define(X, xx).
-else.
-define(X, yy).
-endif.  
 
%%asdasdasd
x() -> io:format("~p99999~n", [?X]).
   
 
%asdasdas    
          
-spec round(XX, integer()) -> integer()
  when XX :: atom().

round(Price, buy) ->
    [ {Key, apmath:ceil(Count)} || {Key, Count} <- Price ];
round(Price, sell) ->
    [ {Key, trunc(Count)} || {Key, Count} <- Price ].

