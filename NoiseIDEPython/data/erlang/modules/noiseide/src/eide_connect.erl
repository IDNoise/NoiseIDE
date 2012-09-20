-module(eide_connect).
 
-export([
    start/1,
    loop/1,
    accept/1,
    prop/1, 
    prop/2, 
    worker/0,
    send/1,
    set_prop/2 
]). 

-record(state, {
    workers,
    fly_compiler,
    erlang_cache_generator, 
    socket
}).

-define(noreply, noreply).

start(Port) ->
    {ok, LS} = gen_tcp:listen(Port, [binary, {active, false}, {packet, 4}]), %{reuseaddr, true},
    io:format("started on:~p~n", [Port]),
    ets:new(props, [named_table, public]),
    Pid = spawn(fun() -> accept(LS) end),
    register(?MODULE, Pid).  

accept(LS) ->
    gen_tcp:controlling_process(LS, self()),
    {ok, Socket} = gen_tcp:accept(LS),
    io:format("accepted~n"),
    AcceptResponce = mochijson2:encode({struct, [{response, connect}]}), 
    gen_tcp:send(Socket, AcceptResponce),
    State = #state{
        workers = [spawn(fun worker/0) || _ <- lists:seq(1, 30)],
        fly_compiler = spawn(fun worker/0),
        erlang_cache_generator = spawn(fun worker/0),
        socket = Socket},
    loop(State).
     
send(Data) ->
    ?MODULE ! {send, Data}.
    
loop(State) ->
    %io:format("loop~n"),
    inet:setopts(State#state.socket, [{active,once}]),
    receive
        {send, Data} ->
            %io:format("Send:~p~n", [Data]),
            gen_tcp:send(State#state.socket, Data),
            loop(State);
        {tcp, Socket, Data} ->
            {Action, ActionData} = process(Data),
            case Action of
                compile_file_fly -> 
                    catch erlang:exit(State#state.fly_compiler, kill),
                    State1 = State#state{fly_compiler = spawn(fun worker/0)},
                    State1#state.fly_compiler ! {{Action, ActionData}, Socket},
                    loop(State1);
                gen_erlang_cache -> 
                    catch erlang:exit(State#state.erlang_cache_generator, kill),
                    State1 = State#state{erlang_cache_generator = spawn(fun worker/0)},
                    State1#state.erlang_cache_generator ! {{Action, ActionData}, Socket},
                    loop(State1); 
                Action when Action == add_path; Action == remove_path;
                    Action == set_prop; Action == remove_prop ->
                    execute_instant_action(Action, ActionData),
                    loop(State);
                _ ->
                    [W|T] = State#state.workers,
                    W ! {{Action, ActionData}, Socket},
                    loop(State#state{workers = T ++ [W]})
            end;
        {tcp_closed, Socket} ->
            io:format("Socket ~w closed [~w]~n",[Socket, self()]),
            ok
    after 5 ->
        loop(State)
    end.

worker() -> 
    receive 
        {{Action, ActionData}, Socket} ->
            try
                Answer = execute_action(Action, ActionData),
                case Answer of
                    ?noreply -> ignore;
                    _ ->
                        %io:format("Answer:~p~n", [byte_size(term_to_binary(Answer))]),
                        gen_tcp:send(Socket, Answer)
                end
            catch Error:Reason -> 
                    %ok
                    io:format("Error:~p, Reason:~p, Trace:~n~p~n", [Error, Reason, erlang:get_stacktrace()])
            end;
        Msg ->
            io:format("Unknown message in worker:~p~n", [Msg])
    after 5000 -> ok
    end,
    worker().

process(Data) -> 
    %io:format("Receved: '~p'~n", [Data]),
    {struct, Json} = mochijson2:decode(Data),
    Action = binary_to_existing_atom(proplists:get_value(<<"action">>, Json), latin1),
    ActionData = proplists:get_value(<<"data">>, Json),
    {Action, ActionData}.


done(Type) -> done(Type, []).
done(Type, Params) -> iolist_to_binary(lists:flatten(mochijson2:encode({struct, [{response, Type}|Params]}))).

execute_instant_action(add_path, PathBinary) ->
    Path = binary_to_list(PathBinary),
    %io:format("add path~p~n", [Path]), 
    code:add_patha(Path),
    eide_compiler:generate_includes(); 
execute_instant_action(remove_path, PathBinary) ->
    Path = binary_to_list(PathBinary),
    code:del_path(Path); 
execute_instant_action(set_prop, Binary) ->
    [Prop, Value] = Binary,
    Key = binary_to_atom(Prop, latin1),
    Val = binary_to_list(Value),
    set_prop(Key, Val), 
    %io:format("set p~p~n", [{Key, Val}]), 
    eide_compiler:generate_includes();
execute_instant_action(remove_prop, Binary) ->
    Key = binary_to_list(Binary),
    ets:delete(props, Key).


execute_action(gen_erlang_cache, RuntimeBinary) ->
    eide_cache:gen_erlang_cache(binary_to_list(RuntimeBinary)),
    done(gen_erlang_cache);
execute_action(rpc, Binary) -> 
    [ModuleB, FunB] = Binary,
    Module = binary_to_atom(ModuleB, latin1),
    Fun = binary_to_atom(FunB, latin1),
    Module:Fun(),
    ?noreply;
execute_action(compile_file_fly, Binary) -> 
    [RealPath, NewPath] = Binary,
    %io:format("compile_file_fly~p~n", [RealPath]), 
    eide_compiler:compile_file_fly(binary_to_list(RealPath), binary_to_list(NewPath));   
execute_action(gen_project_cache, _Binary) ->
    eide_cache:gen_project_cache(),
    done(gen_project_cache);
execute_action(gen_file_cache, Binary) ->
    File = binary_to_list(Binary),
    eide_cache:gen_file_cache(File),
    ?noreply;
execute_action(compile_file, PathBinary) ->
    Path = binary_to_list(PathBinary),
    %io:format("compile_file~p~n", [Path]),  
    eide_compiler:compile_simple(Path);
execute_action(compile_option, Data) ->
    [FileName, App, Option] = Data,
    eide_compiler:compile_with_option(binary_to_list(FileName), binary_to_list(App), binary_to_atom(Option, latin1));
execute_action(compile_project_file, PathBinary) ->
    [FileName, App] = PathBinary,
    %io:format("compile_project_file~p~n", [{FileName, App}]), 
    eide_compiler:compile(binary_to_list(FileName), binary_to_list(App));
execute_action(Action, Data) ->
    io:format("Unknown action ~p with data ~p~n", [Action, Data]),
    ?noreply.

set_prop(Prop, Value) ->
    ets:insert(props, {Prop, Value}).

prop(Prop) ->
    case ets:lookup(props, Prop) of
        [] -> undefined;
        [{Prop, Value}] -> Value
    end.

prop(Prop, Default) ->
    case ets:lookup(props, Prop) of
        [] -> Default;
        [{Prop, Value}] -> Value
    end.