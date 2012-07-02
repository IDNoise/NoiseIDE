-module(eide_connect).
 
-export([
    start/1,
    loop/3,
    accept/1,
    prop/1, 
    worker/0 
]). 
 

-define(noreply, noreply).

start(Port) ->
    {ok, LS} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, 2}]),
    %accept(LS). 
    spawn(fun() -> accept(LS) end). 

accept(LS) ->
    gen_tcp:controlling_process(LS, self()),
    {ok, Socket} = gen_tcp:accept(LS),
    ets:new(props, [named_table, public]),
    %io:format("accepted"),
    AcceptResponce = mochijson2:encode({struct, [{response, connect}]}), 
    gen_tcp:send(Socket, AcceptResponce),
    Workers = [spawn(fun worker/0) || _ <- lists:seq(1, 30)],
    loop(Socket, Workers, spawn(fun worker/0)).
    
loop(Socket, Workers, FlyCompiler) ->
    inet:setopts(Socket, [{active,once}]),
    receive
        {tcp, Socket, Data} ->
            {Action, ActionData} = process(Data),
            case Action of
                compile_file_fly -> 
                    catch erlang:exit(FlyCompiler, kill),
                    FlyCompiler1 = spawn(fun worker/0),
                    FlyCompiler1 ! {{Action, ActionData}, Socket},
                    loop(Socket, Workers, FlyCompiler1);
                _ ->
                    [W|T] = Workers,
                    W ! {{Action, ActionData}, Socket},
                    loop(Socket, T ++ [W], FlyCompiler)
            end;
        {tcp_closed, Socket} ->
            io:format("Socket ~w closed [~w]~n",[Socket, self()]),
            ok
    end.

worker() -> 
    receive 
        {{Action, ActionData}, Socket} ->
            try
                Answer = execute_action(Action, ActionData),
                case Answer of
                    ?noreply -> ignore;
                    _ ->
                        %io:format("Answer:~p~n", [Answer]),
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

execute_action(add_path, PathBinary) ->
    Path = binary_to_list(PathBinary),
    code:add_patha(Path),
    ?noreply; 
execute_action(remove_path, PathBinary) ->
    Path = binary_to_list(PathBinary),
    code:del_path(Path),
    ?noreply; 
execute_action(set_prop, Binary) ->
    [Prop, Value] = Binary,
    Key = binary_to_atom(Prop, latin1),
    Val = binary_to_list(Value),
    ets:insert(props, {Key, Val}),
    ?noreply;
execute_action(remove_prop, Binary) ->
    Key = binary_to_list(Binary),
    ets:delete(props, Key),
    ?noreply;
execute_action(gen_erlang_cache, _Binary) -> 
    eide_cache:gen_erlang_cache(),
    done(gen_erlang_cache);
execute_action(rpc, Binary) -> 
    [ModuleB, FunB] = Binary,
    Module = binary_to_atom(ModuleB, latin1),
    Fun = binary_to_atom(FunB, latin1),
    Module:Fun(),
    ?noreply;
execute_action(compile_file_fly, Binary) -> 
    [RealPath, NewPath] = Binary,
    eide_compiler:compile_file_fly(binary_to_list(RealPath), binary_to_list(NewPath));   
execute_action(gen_project_cache, _Binary) ->
    eide_cache:gen_project_cache(),
    done(gen_project_cache);
execute_action(gen_file_cache, Binary) ->
    File = binary_to_list(Binary),
    eide_cache:gen_file_cache(File),
    done(gen_file_cache, [{path, iolist_to_binary(File)}]);
execute_action(compile_file, PathBinary) ->
    Path = binary_to_list(PathBinary),
    eide_compiler:compile_simple(Path);
execute_action(compile_project_file, PathBinary) ->
    [FileName, App, AppsPath] = PathBinary,
    eide_compiler:compile(binary_to_list(FileName), binary_to_list(App), binary_to_list(AppsPath));
execute_action(Action, Data) ->
    io:format("Unknown action ~p with data ~p~n", [Action, Data]),
    undefined.

prop(Prop) ->
    case ets:lookup(props, Prop) of
        [] -> 
            %io:format("Prop is not set: ~p~n", [Prop]), 
            undefined;
        [{Prop, Value}] -> Value
    end.