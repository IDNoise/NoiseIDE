-module(eide_compiler).

-export([
    compile/2, 
    compile_simple/1,
    compile_file_fly/2,
    generate_includes/0,
    compile_yecc/1
]).  

generate_includes() ->
    Includes = 
        case eide_connect:prop(project_dir) of
            undefined -> 
                [];
            AppsPath ->
                {ok, Apps} = file:list_dir(AppsPath),
                [{i, "../include"}| [{i,Ai} || A <- Apps, End <- ["src", "include"],
                 begin
                     Ai = AppsPath ++ "/"++ A ++"/" ++ End,
                     filelib:is_dir(Ai)
                 end]]
    end,
    %io:format("includes:~p~n", [Includes]),
    eide_connect:set_prop(includes, Includes). 
 
compile(FileName, App) ->
    OutDir = eide_connect:prop(project_dir) ++ "/" ++ App ++ "/ebin",
    %OutputFileName = OutDir ++ "/" ++ filename:rootname(filename:basename(FileName)) ++ ".beam",
    Includes = eide_connect:prop(includes),
    catch file:make_dir(OutDir),  
    create_response(FileName, compile_internal(FileName, [{outdir, OutDir} | Includes])).

compile_simple(FileName) -> 
    erlang:display(FileName),
    case filename:extension(FileName) of
        ".erl" -> 
            create_response(FileName, compile_internal(FileName, [{outdir, filename:dirname(FileName)}]));
        ".yrl" -> 
            compile_yecc(FileName),
            create_response(FileName, [])
    end.
    
compile_yecc(FileName) ->
    ModuleName = filename:rootname(FileName) ++ ".erl",
    yecc:yecc(FileName, ModuleName).
    
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
    case FileName == RealPath of
        true -> spawn(eide_cache, gen_file_cache, [FileName]);
        _ -> spawn(eide_cache, create_cache_file_fly, [FileName, RealPath])
    end,
    [[io:format("Error on compile ~p: ~p~n", [FileName, Er])|| Er <- Err, element(1, Er) == none]|| {_File, Err} <- E],

    Errs = 
        [[begin
            case Er of
                {compile,{module_name, MName, FName}} ->
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
    %io:format("~p~n",[lists:append(Errs) ++ lists:append(Warns)]),
    lists:append(Errs) ++ lists:append(Warns).