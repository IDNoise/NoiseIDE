%% @author: [username]
%% @date: [date]

-module([module_name]).

-behaviour(gen_event).

%% API
-export([
	start_link/0, 
	add_handler/0
]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler() ->
    gen_event:add_handler(?MODULE, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================


init([]) ->
    {ok, #state{}}.

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



