%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 09.09.2012

-module(test_cache_module). 
   
%% Include files  
-include_lib("noiseide/include/props.hrl").
-include_lib("kernel/include/file.hrl").
%-include_lib("sample.hrl").
%% Exported Functions 
 
-export([ 
    x/0, 
    function/1,
    xxx/0,
    foldl/3,
    member/2
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
%%%================ ============  ======= ================================
      
-record(ololo, {         
    field_0,  
    field_1 = 1 :: beam_instr(),  
    field_2 :: atom() | boolean() | ololoxxx,
    field_3 :: dialyzer_plt:plt_info()
}).       
      
-record(state,
        {state %% of RFC 3588 Peer State Machine
              :: {'Wait-Conn-Ack', atom()}
               | recv_CER
               | {'Wait-CEA', atom(), atom()}
               | 'Open',
         mode :: accept | connect | {connect, reference()},
         parent       :: pid(),     %% watchdog process
         transport    :: pid(),     %% transport process
         dictionary   :: module(),  %% common dictionary
         service      :: atom() | undefined,
         dpr = false  :: false 
                       | true  %% DPR received, DPA sent
                       | {boolean(), atom(), atom()},
                       %% hop by hop and end to end identifiers in
                       %% outgoing DPR; boolean says whether or not
                       %% the request was sent explicitly with
                       %% diameter:call/4.
         codec :: #{decode_format := diameter:decode_format(),
                    string_decode := boolean(),
                    strict_mbit := boolean(),
                    rfc := 3588 | 6733, 
                    ordered_encode := false},
         strict :: boolean(),
         ack = false :: boolean(), 
         length_errors :: exit | handle | discard,
         incoming_maxlen :: integer() | infinity}).
      
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
    
    Z = #state{
        ack = 1
        },
    SortedQueueDiffs = 
        lists:sort(fun({{_, IR1}, D1}, {{_, IR2}, D2}) -> 
            Add1 = case IR1 of true -> 1; _ -> 0 end, 
            Add2 = case IR2 of true -> 1; _ -> 0 end, 
            D1 * 1000 + Add1 < D2 * 1000 + Add2 
        end, []),
        replace_labels(x),
    io:format("~p~n", [?X]).
     
-spec round(Price, integer()) -> integer()  
  when Price :: atom(). 
%% @doc Prints the value X. 
%round(Price, buy) -> 
%    [ {Key, apmath:ceil(Count)} || {Key, Count} <- Price ];
round(Price, sell) -> 
    [ {Key, trunc(Count)} || {Key, Count} <- Price ].
 

-spec replace_labels(#{type := atom(),
                        id :=  atom(),
                        counters :=  atom()}) -> [atom()].
replace_labels(Is) -> test_cache_module1:xxroundxx(x, x). 
 
-spec foldl(Fun, Acc0, List) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List :: [T],
      T :: term().

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) when is_function(F, 2) -> Accu.

-spec member(Elem, List) -> boolean() when
      Elem :: T,
      List :: [T],
      T :: term().

member(_, _) ->
    erlang:nif_error(undef).