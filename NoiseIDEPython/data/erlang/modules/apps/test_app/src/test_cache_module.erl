%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 09.09.2012

-module(test_cache_module). 
   
%% Include files  
-include_lib("noiseide/include/props.hrl").
%-include_lib("sample.hrl").
%% Exported Functions 
 
-export([ 
    x/0, 
    function/1 
]).    
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore. 

-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback on_execute(State, Command, Args) -> State when
      Command :: atom(),
      Args :: [binary()].

-callback on_clear_lobby(State) -> State. 

-export([

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
function(<<A,B,C,D>>) -> x.
%% @doc Prints the value X.
xxx() -> x.
    %?XX + ?XssssX.  

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

