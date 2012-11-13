-module(eide_compiler).

-include_lib("kernel/include/file.hrl").

-export([
    compile/1,
    compile_file_fly/2,
    generate_includes/0,
    compile_yecc/1,
    compile_with_option/3
]).  

generate_includes() ->
    Includes = 
        case eide_connect:prop(project_dir) of
            undefined -> 
                [];
            AppsPath ->
                {ok, Apps} = file:list_dir(AppsPath),
                [{i, "../include"}| [{i,Ai} || A <- Apps, End <- ["include"],
                 begin
                     Ai = AppsPath ++ "/"++ A ++"/" ++ End,
                     filelib:is_dir(AppsPath ++ "/"++ A) 
                 end]]
    end, 
    %io:format("includes:~p~n", [Includes]),
    eide_connect:set_prop(includes, Includes),
    FlatIncludes = lists:map(fun({i, F}) -> F end, Includes),
    eide_connect:set_prop(flat_includes, FlatIncludes). 
  
compile(FileName) -> 
    case filename:extension(FileName) of
        ".erl" -> 
            OutDir = eide_connect:prop(project_dir) ++ "/" ++ app_name(FileName) ++ "/ebin",
            Includes = eide_connect:prop(includes),
            catch file:make_dir(OutDir),  
            create_response(FileName, compile_internal(FileName, [{outdir, OutDir} | Includes]));
        ".yrl" -> 
            compile_yecc(FileName),
            create_response(FileName, [])
    end.
    
%d:/projects/noiseide/noiseidepython/data/erlang/modules/noiseide/src/eide_compiler.erl
%eide_compiler:compile_with_option("d:/projects/noiseide/noiseidepython/data/erlang/modules/noiseide/src/eide_compiler.erl", "noiseide", 'S').
compile_with_option(FileName, App, Option) -> 
    OutDir = eide_connect:prop(project_dir) ++ "/" ++ App ++ "/ebin",
    Includes = eide_connect:prop(includes),
    compile:file(FileName, [{outdir, OutDir}, Option | Includes]),
    File = OutDir ++ "/" ++ filename:rootname(filename:basename(FileName)) ++ "." ++ atom_to_list(Option),
    {ok, Data} = file:read_file(File),
    mochijson2:encode({struct, [{response, compile_option},
                                {option, Option},
                                {path, iolist_to_binary(FileName)},
                                {result, iolist_to_binary(Data)}]}).

compile_yecc(FileName) ->
    %ModuleName = filename:rootname(FileName) ++ ".erl",
    Result = yecc:file(FileName, [report]),
    %io:format("yecc result ~p~n", [Result]),
    case Result of
        {ok, ModuleName} ->
            Response = eide_compiler:compile(ModuleName),
            eide_connect:send(Response), 
			send_yecc_response(FileName, warning, []),
			noreply;
        {ok, ModuleName, Warnings} ->
            Response = eide_compiler:compile(ModuleName),
            eide_connect:send(Response),
            send_yecc_errors(warning, Warnings), 
            noreply;
        {error, Warnings, Errors} ->
            send_yecc_errors(warning, Warnings),
            send_yecc_errors(error, Errors),
            noreply;
        _ ->
            mochijson2:encode({struct, [{response, compile}, 
										{errors, [[{type, error}, {line, 0}, 
												   {msg, iolist_to_binary("Unknown compilation error")}]]}, 
										{path, iolist_to_binary(FileName)}]})
    end.
 
app_name(ModuleName) ->
    Elements = filename:split(ModuleName),
    lists:last(lists:takewhile(fun(E) -> E =/= "src" end, Elements)).
    
send_yecc_errors(Type, Errors) ->
    [send_yecc_response(File, Type, ErrorsInfo) || {File, ErrorsInfo} <- Errors].

send_yecc_response(File, Type, ErrorsInfo) ->
    Errors = 
        [begin
            Msg = iolist_to_binary(Module:format_error(Error)),
            [{type, Type}, {line, Line}, {msg, Msg}]
         end || {Line, Module, Error} <- ErrorsInfo],
    Response = 
        mochijson2:encode({struct, [{response, compile}, 
                                    {errors, Errors}, 
                                    {path, iolist_to_binary(File)}]}),
    eide_connect:send(Response).  
    
default_options() ->
    [ 
     warn_obsolete_guard, 
     %warn_unused_import,
     warn_shadow_vars, 
     warn_export_vars, 
     debug_info,
     return_errors, 
     return_warnings,   
     strong_validation
    ].

create_response(FilePath, Errors) -> 
    create_response(FilePath, Errors, []).
create_response(FilePath, Errors, Args) -> 
    mochijson2:encode({struct, [{response, compile}, 
                                {errors, Errors}, 
                                {path, iolist_to_binary(FilePath)},
                                {args, Args}]}).
create_response_fly(FilePath, Errors) -> 
    mochijson2:encode({struct, [{response, compile_fly}, 
                                {errors, Errors}, 
                                {path, iolist_to_binary(FilePath)}]}).

compile_file_fly(RealPath, NewPath) -> 
    create_response_fly(RealPath, 
                    compile_internal(NewPath, eide_connect:prop(includes), false, RealPath)).

parse_term(String) when is_binary(String) ->
    parse_term(binary_to_list(String));
parse_term(String) when is_list(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.



compile_internal(FileName, Options) ->
    compile_internal(FileName, Options, true, FileName).
compile_internal(FileName, Options, ToBinary, RealPath) ->
    Options0 = default_options() ++ Options,
    Options1 = 
        case eide_connect:prop(compiler_options) of
            undefined -> Options0;
            Str -> Options0 ++ parse_term("[" ++ Str ++ "].")
        end, 
    Options2 = case ToBinary of 
                   true -> 
                       [debug_info | Options1] -- [strong_validation];
                   _ -> Options1
               end, 
    Result = compile:file(FileName, Options2),
    {E, W} = case Result of 
                 {ok, _Module, Warnings} ->
                     {[], Warnings};
                 {error, Errors, Warnings} ->
                     {Errors, Warnings}
             end,
    %io:format("Spawn~p~n", [FileName == RealPath]),
    case FileName == RealPath of
        true -> spawn(eide_cache, gen_file_cache, [FileName]);
        _ -> spawn(eide_cache, create_cache_file_fly, [FileName, RealPath])
    end,
%    [[io:format("Error on compile ~p: ~p~n", [FileName, Er])|| Er <- Err, element(1, Er) == none]|| {_File, Err} <- E],
    %io:format("Error on compile ~p: ~p~n", [FileName, {E, W}]),
    Errs = 
        [[begin
            case Er of
                {compile, write_error} ->
                    [{type, error}, {line, 0}, {msg, iolist_to_binary("Error with writing file.")}];
                {compile,{module_name, MName, FName}} ->
%                    io:format("mn~p~n", [{MName, FName, FileName, file:read_file(FileName)}]),
                    [{type, error}, {line, 2}, {msg, iolist_to_binary("Module in file '" ++ FName ++ 
                        "' has wrong name: '" ++ atom_to_list(MName) ++ "'.")}];
                {Line, M, Error}  ->
                  Msg = iolist_to_binary(M:format_error(Error)),
                  [{type, error}, {line, Line}, {msg, Msg}]
            end 
          end || Er <- Err, element(1, Er) =/= none] 
         || {_File, Err} <- E],  
    Warns = 
        [[begin
              {Line, M, Error} = Wa,
              Msg = iolist_to_binary(M:format_error(Error)),
              [{type, warning}, {line, Line}, {msg, Msg}]
          end || Wa <- War] 
         || {_File, War} <- W],
%    case Errs of
%        [] -> ok;
%        _ ->
%            io:format("Errs~p~n~p~n", [Errs, {FileName, FileInfo#file_info.size, file:read_file(FileName)}])
%    end,
    %io:format("~p~n",[lists:append(Errs) ++ lists:append(Warns)]),
    lists:append(Errs) ++ lists:append(Warns).