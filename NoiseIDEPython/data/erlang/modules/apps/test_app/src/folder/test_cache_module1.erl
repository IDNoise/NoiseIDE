%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 09.09.2012

-module(test_cache_module1). 
-behavior(test_cache_module).   
%% Include files 
-include("sample.hrl").    
-include_lib("noiseide/include/props.hrl").
%% Exported Functions 

-export([   
    x/0,
    xxxyyy/0,  
    xxroundxx/2  
]).    
-define(XssssX, ololasdasdado).  
-type beam_instr() :: 'bs_init_writable' | 'fclearerror' | 'if_end'
                    | 'remove_message' | 'return' | 'send' | 'timeout'
                    | tuple().  %% XXX: Very underspecified - FIX THIS 
   

-spec xxroundxx(XX, integer()) -> integer()
  when XX :: atom().
%% @doc Prints the value X.
%xxroundxx(Price, buy) ->
%    [ {Key, apmath:ceil(Count)} || {Key, Count} <- Price ];
xxroundxx(Price, sell) ->
    [ {Key, trunc(Count)} || {Key, Count} <- Price ].
 
%%%========================== ==== ======= ==============================
%%% API  

%-include("sample.hrl
%%%========================== =  ======= ================================
      
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
xxxyyy() ->
    ?XX + ?XssssX.  

-ifdef(debug).
-define(X, xx).
-else.
-define(X, yy).
-endif.  
  
x() -> 
    xxxyyy(),
    ?MODULE:xxxyyy(),
    fun ?MODULE:xxxyyy/0,
    SortedQueueDiffs = 
        lists:sort(fun({{_, IR1}, D1}, {{_, IR2}, D2}) -> 
            Add1 = case IR1 of true -> 1; _ -> 0 end, 
            Add2 = case IR2 of true -> 1; _ -> 0 end, 
            D1 * 1000 + Add1 < D2 * 1000 + Add2 
        end, []),
    io:format("~p~n", [?X]).

-spec init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.

init(Args) ->
    erlang:error({not_implemented, init}).
