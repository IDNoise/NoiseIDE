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
    {ok, LS} = gen_tcp:listen(Port, [binary, {active, false}, {packet, 4}, {sndbuf, 4096}]), %{reuseaddr, true},
    io:format("started on:~p~n", [Port]),
    ets:new(props, [named_table, public]),
    Pid = spawn(fun() -> accept(LS) end), 
    register(?MODULE, Pid),
    {ok, SimpleParamRe} = re:compile(<<"[A-Za-z_0-9,\s]*">>),
    eide_connect:set_prop(simple_param_re, SimpleParamRe),
    {ok, DocRe} = re:compile(<<"(<p><a name=.*?</div>\\s*(?=<p><a name=|<div class=\"footer\">))">>, [multiline, dotall]),
    eide_connect:set_prop(doc_re, DocRe),
    {ok, PartDocRe} = re:compile(<<"<a name=\"([A-Za-z_:]*?)-([0-9]?)\">(?:</a>)?<span.*?>(.*?)\\((.*?)\\)\\s*-&gt;\\s*(.*?)</span>">>),
    eide_connect:set_prop(part_doc_re, PartDocRe),
    {ok, EDocRe} = re:compile(<<"(<h3 class=\"function\"><a name=.*?</div>(?=\n\n<h3 class=\"function\"><a name=|\n<hr>))">>, [multiline, dotall]),
    eide_connect:set_prop(edoc_re, EDocRe),
    {ok, PartEDocRe} = re:compile(<<"<h3 class=\"function\"><a name=\"([A-Za-z_:]*?)-([0-9]?)\">">>),
    eide_connect:set_prop(part_edoc_re, PartEDocRe),
    Pid.  

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
    inet:setopts(State#state.socket, [{active,once}]),
    receive
        {send, Data} ->
            %io:format("Send:~p~n", [Data]),
            gen_tcp:send(State#state.socket, Data),
            loop(State);
        {tcp, Socket, Data} ->
            {Action, ActionData} = process(Data),
            %io:format("TCP: ~p -> ~p~n", [Action, ActionData]),
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
    after 1 ->
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
                        %io:format("Answer:~p~n", [Answer]),
                        gen_tcp:send(Socket, Answer)
                end
            catch 
                exit:{ucs, {bad_utf8_character_code}} ->
                    ok;
                Error:Reason:Stacktrace -> 
                    %ok
                    io:format("Error:~p, Reason:~p, Data: ~p~nTrace:~n~p~n", [Error, Reason, {Action, ActionData}, Stacktrace])
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
    code:add_patha(Path); 
execute_instant_action(remove_path, PathBinary) ->
    Path = binary_to_list(PathBinary),
    code:del_path(Path); 
execute_instant_action(set_prop, Binary) ->
    [Prop, Value] = Binary,
    Key = binary_to_atom(Prop, latin1),
    Val = binary_to_list(Value),
    set_prop(Key, Val);
execute_instant_action(set_home, Binary) ->
    Path = binary_to_list(Binary),
    os:putenv("HOME", Path);
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
    eide_compiler:compile_file_fly(binary_to_list(RealPath), binary_to_list(NewPath));   
execute_action(gen_project_cache, _Binary) ->
    eide_cache:gen_project_cache(),
    done(gen_project_cache);
execute_action(gen_file_cache, Binary) ->
    File = binary_to_list(Binary),
    eide_cache:gen_file_cache(File),
    ?noreply;
execute_action(compile, PathBinary) ->
    Path = binary_to_list(PathBinary), 
    eide_compiler:compile(Path);
execute_action(compile_tests, PathBinary) ->
    Path = binary_to_list(PathBinary),
    CompileResults = eide_compiler:compile_tests(Path),
    done(compile_app, [{path, PathBinary}, {result, CompileResults}]);
execute_action(compile_app, PathBinary) ->
    Path = binary_to_list(PathBinary),
    CompileResults = eide_compiler:compile_app(Path),
    done(compile_app, [{path, PathBinary}, {result, CompileResults}]);
execute_action(cache_app, PathBinary) ->
    Path = binary_to_list(PathBinary),
    eide_cache:cache_app(Path),
    done(cache_app, [{path, PathBinary}]);
execute_action(compile_option, Data) ->
    [FileName, Option] = Data,
    eide_compiler:compile_with_option(binary_to_list(FileName), binary_to_atom(Option, latin1));
execute_action(xref_module, Binary) ->
    Module = 
        try 
            binary_to_existing_atom(Binary, latin1)
        catch _:_ ->
            binary_to_atom(Binary, latin1)
        end,
    xref_module(Module);
execute_action(dialyze_modules, Binary) ->
    Modules = [binary_to_list(Path) || Path <- Binary],
    dialyze(files, Modules);
execute_action(dialyze_apps, Binary) ->
    Apps = [binary_to_list(Path) || Path <- Binary],
    dialyze(files_rec, Apps);
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

xref_module(Module) ->
    UndefinedData = 
        case xref:m(Module) of
            [_, {undefined, Undefined}, _] ->
                [{struct, [{where_m, WM}, {where_f, WF}, {where_a, WA}, 
                 {what_m, M}, {what_f, F}, {what_a, A}]} 
                 || {{WM, WF, WA}, {M, F, A}} <- Undefined];
            _ ->
                []
        end,
    Response = {struct, [
                    {response, xref_module},
                    {module, atom_to_binary(Module, latin1)},
                    {undefined, UndefinedData}
                ]},
    mochijson2:encode(Response).

dialyze(Type, FilesApps) ->
    Plt = prop(plt),
    case os:getenv("HOME") of
        false ->
            os:putenv("HOME", prop(project_dir));
        _ ->
            ok
    end,
    Options = [
        {Type, FilesApps}, 
        {init_plt, Plt}, 
        {get_warnings, true}
    ],
    Warnings = dialyzer:run(Options),
    WarningsStrings = [list_to_binary(dialyzer:format_warning(W)) || W <- Warnings],
    Response = {struct, [
                    {response, dialyzer},
                    {warnings, WarningsStrings}
                ]},
    mochijson2:encode(Response).