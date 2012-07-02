-module(eide_compiler).

-export([
    compile/3, 
    compile_simple/1,
    compile_file_fly/2,
    generate_includes/1
]). 

generate_includes(AppsPath) ->
    {ok, Apps} = file:list_dir(AppsPath),
    [{i, "../include"}| [{i,Ai} || A <- Apps, End <- ["src", "include"],
                         begin
                             Ai = AppsPath ++ "/"++A++"/" ++ End,
                             filelib:is_dir(Ai)
                         end]]. 

compile(FileName, App, AppsPath) -> 
    OutDir = AppsPath ++ "/" ++ App ++ "/ebin",
    %OutputFileName = OutDir ++ "/" ++ filename:rootname(filename:basename(FileName)) ++ ".beam",
    Includes = generate_includes(AppsPath),
    catch file:make_dir(OutDir),
    create_response(FileName, compile_internal(FileName, [{outdir, OutDir} | Includes])).

compile_simple(FileName) ->
    create_response(FileName, compile_internal(FileName, [{outdir, filename:dirname(FileName)}])).

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

compile_file_fly(RealPath, NewPath) -> 
    Includes = 
        case eide_connect:prop(project_dir) of
            undefined -> [];
            Dir -> generate_includes(Dir)
        end,
    create_response(RealPath, 
                    compile_internal(NewPath, Includes, false, RealPath)).

compile_internal(FileName, Options) ->
    compile_internal(FileName, Options, true, FileName).
compile_internal(FileName, Options, ToBinary, RealPath) ->
    Options1 = default_options() ++ Options,
    Options2 = case ToBinary of 
                   true -> 
                       [debug_info | Options1] -- [strong_validation];
                   _ -> Options1
               end,
    Result = compile:file(FileName, Options2),
    {E, W} = case Result of 
                 {ok, _Module, Warnings} ->
                     case FileName == RealPath of
                         true -> eide_cache:gen_file_cache(FileName);
                         _ -> eide_cache:create_cache_file_fly(FileName, RealPath)
                     end,
                     {[], Warnings};
                 {error, Errors, Warnings} ->
                     {Errors, Warnings}
             end,
    Errs = 
        [[begin
              {Line, M, Error} = Er,
              Msg = iolist_to_binary(M:format_error(Error)),
              [{type, error}, {line, Line}, {msg, Msg}]
          end || Er <- Err] 
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