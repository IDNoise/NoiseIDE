-module(eide_compiler).

-include_lib("kernel/include/file.hrl").

-export([
    compile/1, 
    compile_file_fly/2, 
    compile_yecc/1,
    compile_with_option/2,
    compile_app/1,
    app_out_dir/1,
    compile_tests/1,
    app_name/1,
    app_path/1
]).   

compile(FileName) -> 
    case filename:extension(FileName) of
        ".erl" -> 
            OutDir = app_out_dir(FileName),
            catch file:make_dir(OutDir), 
            create_response(FileName, compile_internal(FileName, [{outdir, OutDir}]));
        ".yrl" -> 
            compile_yecc(FileName),
            create_response(FileName, []);
        ".src" -> 
            case filename:extension(filename:basename(FileName, ".src")) of
                ".app" ->
                    compile_appsrc(FileName),
                    create_response(FileName, []);
                _ ->
                    noreply
            end
    end.
    
compile_app(AppPath) ->
    OutDir = AppPath ++ "/ebin",
    catch file:make_dir(OutDir),
    SrcDir = AppPath ++ "/src",
    HrlDir = AppPath ++ "/include",
    {Modules, LocalHrls, Yrls, AppSrcFile} = filelib:fold_files(SrcDir, ".*\.(erl|hrl|yrl|app\.src)$", true, 
        fun(File, {M, H, Y, ASF}) ->
           case filename:extension(File) of
               ".erl" -> {[File|M], H, Y, ASF};
               ".hrl" -> {M, [File|H], Y, ASF};
               ".yrl" -> {M, H, [File|Y], ASF};
               ".src" -> {M, H, Y, File}
           end
        end, {[], [], [], undefined}), 
    IncludeHrls = filelib:fold_files(HrlDir, ".*\.hrl$", true, 
        fun(File, H) ->
           case filename:extension(File) of
               ".hrl" -> [File|H];
               _ -> H
           end
        end, []), 
    ModuleNames = [filename:rootname(filename:basename(M)) || M <- Modules],
    filelib:fold_files(OutDir, ".*\.(beam)$", true, 
        fun(File, _) -> 
            case lists:member(filename:rootname(filename:basename(File)), ModuleNames) of
                true -> ok;
                false -> file:delete(File) 
            end
        end, undefined),        
    [eide_cache:gen_file_cache(H) || H <- IncludeHrls ++ LocalHrls],
    Modules1 = Modules ++ [filename:rootname(Y) ++ ".erl" || Y <- Yrls, compile_yecc(Y) == ok],
    SrcResult = [compile_result(M, OutDir) || M <- Modules1],
    
    case AppSrcFile of
        undefined -> ignore;
        _ -> compile_appsrc(AppSrcFile)
    end,
    SrcResult.
 
compile_tests(AppPath) ->
    OutDir = AppPath ++ "/ebin",
    catch file:make_dir(OutDir),
    TestDir = AppPath ++ "/test",
    TestResult = filelib:fold_files(TestDir, ".*\.erl$", true, 
        fun(File, R) ->
           [compile_result(File, OutDir) | R]
        end, []), 
    TestResult.
 
compile_result(File, OutDir) ->
    catch file:make_dir(OutDir),
    {struct, 
        [{path, iolist_to_binary(File)}, 
         {errors, compile_internal(File, [{outdir, OutDir}])}
        ]}.
 
compile_appsrc(AppSrcFile) ->
    OutDir = app_out_dir(AppSrcFile),
    catch file:make_dir(OutDir),
    AppFile = OutDir ++ "/" ++ filename:basename(AppSrcFile, ".app.src") ++ ".app",
    {ok, SrcData} = file:read_file(AppSrcFile),
    Beams = filelib:fold_files(OutDir, ".*\.beam$", true, 
        fun(File, B) -> 
            [list_to_atom(filename:basename(filename:rootname(File)))| B] 
        end, []),
    SrcData1 = re:replace(SrcData, "{modules,.*?}", io_lib:format("{modules,~p}", [Beams])),
    file:write_file(AppFile, SrcData1).
    
compile_with_option(FileName, Option) ->
    OutDir = app_out_dir(FileName),
    catch file:make_dir(OutDir),
    Options = [{outdir, OutDir}, Option, {i, filename:join(app_path(FileName), "include")}, {i, filename:join(app_path(FileName), "src")}] ++ compiler_options(),
    compile:file(FileName, Options),
    OptionExt = case Option of 
        to_core -> core;
        _ -> Option
    end,
    File = OutDir ++ "/" ++ filename:rootname(filename:basename(FileName)) ++ "." ++ atom_to_list(OptionExt),
    {ok, Data} = file:read_file(File),
    file:delete(File),
    mochijson2:encode({struct, [{response, compile_option},
                                {option, Option},
                                {path, iolist_to_binary(FileName)},
                                {result, Data}]}).

compile_yecc(FileName) ->
    Result = yecc:file(FileName, []),
    case Result of
        {ok, ModuleName} ->
            Response = eide_compiler:compile(ModuleName),
            eide_connect:send(Response), 
			send_yecc_response(FileName, warning, []),
			ok;
        {ok, ModuleName, Warnings} ->
            Response = eide_compiler:compile(ModuleName),
            eide_connect:send(Response),
            send_yecc_errors(warning, Warnings), 
            ok;
        {error, Warnings, Errors} ->
            send_yecc_errors(warning, Warnings),
            send_yecc_errors(error, Errors),
            error;
        _ ->
            io:format("Unknown yrl compilation error~n"),
            error
    end.
 
app_name(File) -> 
    App = lists:nth(2, lists:dropwhile(
        fun(I) -> 
            not lists:member(I, ["src", "include", "test"])
        end, 
        lists:reverse(filename:split(File)))
    ),
    case lists:member($-, App) of
        true -> lists:takewhile(fun(E) -> E =/= $- end, App);
        _ -> App
    end.
    
app_path(File) ->
    filename:join(lists:takewhile(
        fun(I) -> 
            I =/= "src" andalso I =/= "include" andalso I =/= "test" 
        end, 
        filename:split(File))
    ).

app_out_dir(File) ->
    Path = app_path(File),
    Path ++ "/ebin". 
    
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
     warn_shadow_vars, 
     warn_export_vars, 
     debug_info,
     return,   
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
    create_response_fly(RealPath, compile_internal(NewPath, [{i, filename:join(app_path(RealPath), "include")}], false, RealPath)).

parse_term(String) when is_binary(String) ->
    parse_term(binary_to_list(String));
parse_term(String) when is_list(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

compiler_options() ->
    case eide_connect:prop(compiler_options) of
            undefined -> [];
            Str -> parse_term("[" ++ Str ++ "].")
    end.

compile_internal(FileName, Options) ->
    compile_internal(FileName, Options, true, FileName).
compile_internal(FileName, Options, ToBinary, RealPath) ->
    Options0 = default_options() ++ Options 
        ++ [{i, filename:join(app_path(RealPath), "include")}, {i, filename:join(app_path(RealPath), "src")}] 
        ++ compiler_options(),
    Options1 = case ToBinary of 
                   true -> 
                       [debug_info | Options0] -- [strong_validation];
                   _ -> Options0
               end, 
    Result = compile:file(FileName, Options1),
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
    Errs = 
        [[begin
            case Er of
                {compile, write_error} ->
                    [{type, error}, {line, 0}, {msg, iolist_to_binary("Error with writing file.")}];
                {none, compile,{module_name, MName, FName}} ->
                    [{type, error}, {line, 2}, {msg, iolist_to_binary("Module in file '" ++ FName ++ 
                        "' has wrong name: '" ++ atom_to_list(MName) ++ "'.")}];
                {LineX, M, Error} when is_integer(LineX) orelse is_tuple(LineX)  ->
                    {Line, _Column} = case LineX of {L, C} -> {L, C}; L -> {L, 0} end,
                    Msg = iolist_to_binary(M:format_error(Error)),
                    [{type, error}, {line, Line}, {msg, Msg}];
                _ ->
                    []
            end 
          end || Er <- Err] 
         || {_File, Err} <- E],  
    Warns = 
        [[begin
              {LineX, M, Error} = Wa,
              {Line, _Column} = case LineX of {L, C} -> {L, C}; L -> {L, 0} end,
              Msg = iolist_to_binary(M:format_error(Error)),
              [{type, warning}, {line, Line}, {msg, Msg}]
          end || Wa <- War] 
         || {_File, War} <- W],
    lists:filter(fun(El) -> El =/= [] end, lists:append(Errs) ++ lists:append(Warns)).