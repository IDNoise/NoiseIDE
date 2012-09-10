%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 09.09.2012

-module(test_cache_module).

%% Include files

%% Exported Functions

-export([
     
]).  
-define(XX, ololo).   
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
          