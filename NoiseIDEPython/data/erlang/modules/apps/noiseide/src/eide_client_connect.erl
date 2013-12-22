-module(eide_client_connect).
 
-export([
    start/1,
    loop/1,
    accept/1,
    send/2
]). 

-record(state, {
    socket
}).

-define(noreply, noreply).

start(Port) ->
    {ok, LS} = gen_tcp:listen(Port, [binary, {active, false}, {packet, 4}, {sndbuf, 4096}]), 
    io:format("started on:~p~n", [Port]),
    Pid = spawn(fun() -> accept(LS) end), 
    register(?MODULE, Pid),
    Pid.  

accept(LS) ->
    gen_tcp:controlling_process(LS, self()),
    {ok, Socket} = gen_tcp:accept(LS),
    io:format("accepted~n"),
    State = #state{socket = Socket},
    loop(State).
     
send(Action, Data) ->
    Data1 = {struct, [{action, Action} | Data]}, 
    Data2 = mochijson2:encode(Data1),
    ?MODULE ! {send, Data2},
    ok.
    
loop(State) ->
    inet:setopts(State#state.socket, [{active,once}]),
    receive
        {send, Data} ->
            %io:format("Send:~p~n", [Data]),
            gen_tcp:send(State#state.socket, Data),
            loop(State);
        {tcp_closed, Socket} ->
            io:format("Socket ~w closed [~w]~n",[Socket, self()]),
            ok
    after 5 ->
        loop(State)
    end.