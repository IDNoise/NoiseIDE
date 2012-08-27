%% @author: [username]
%% @date: [date]

-module([module_name]).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
                 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @spec init(Args) -> {ok, State}
%% @end
init([]) ->
    {ok, #state{}}.

%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
handle_event(_Event, State) ->
    {ok, State}.

%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
handle_info(_Info, State) ->
    {ok, State}.

%% @spec terminate(Reason, State) -> void()
%% @end
terminate(_Reason, _State) -> ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



