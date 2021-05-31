-module(eide_cache).

-include_lib("edoc/src/edoc_types.hrl").
-include("props.hrl").
 
-define(log(P), io:format("~p~n", [P])).
-define(TERM, "term()"). 
-define(VAR, "Var"). 
   
-export([  
    create_cache_for_erlang_libs/1,
    create_cache_file_fly/2,
    generate_file/4, 
    gen_file_cache/1, 
    gen_erlang_cache/1, 
    gen_project_cache/0,
    cache_app/1,
    test/0    
]).

-record(function, {  
    name,
    line = 0 :: integer(), 
    params = [],  
    types = [],  
    result, 
    exported,
    doc,
    bif = false,    
    comment = []     
}).

-record(content, {
    module_name :: atom(),  
    file,
    last_file_attr,
    beam = false,
    functions = [],  
    specs = [],
    callbacks = [],
    records = [], 
    macros = [],          
    includes = [],
    exports = [],
    exported_types = [],
    behaviors = []
}).  

-record(spec, {
    name, 
    vars = [], 
    types = [], 
    result
}).

-record(callback, {
    function, 
    params = [], 
    line
}).


-record(record, {
    name, 
    line, 
    fields = [], 
    file
}).

-record(field, {
    name, 
    type = "undefined"
}).

-record(macro, {
    name, 
    value, 
    line, 
    file
}).

test() ->
    generate_from_source("c:/PROGRA~4/erl5.10/lib/wx-1.0/src/gen/wxBoxSizer.erl", "c:/PROGRA~4/erl5.10/lib/wx-1.0/src/gen/wxBoxSizer.erl").

gen_file_cache(File) -> create_cache(eide_connect:prop(cache_dir) ++ "/" ++ eide_connect:prop(project_name), File).  

gen_erlang_cache(Runtime) -> 
    Dir = eide_connect:prop(cache_dir) ++ "/runtimes/" ++ Runtime,
    io:format("Checking cache for erlang libs ~p~n", [Dir]),
    catch file:make_dir(Dir),
    create_cache_for_erlang_libs(Dir),
    io:format("Checking cache for erlang libs......Done~n").

gen_project_cache() ->
    io:format("Checking cache for project..."),
    create_cache(eide_connect:prop(cache_dir) ++ "/" ++ eide_connect:prop(project_name), eide_connect:prop(project_dir), []),
    io:format("...Done").

cache_app(AppPath) ->
    SrcDir = AppPath ++ "/src",
    HrlDir = AppPath ++ "/include",
    ModulesAndHrls = filelib:fold_files(SrcDir, ".*\.(erl|hrl)$", true, 
        fun(File, F) -> [File|F]  end, []), 
    IncludeHrls = filelib:fold_files(HrlDir, ".*\.hrl$", true, 
        fun(File, H) -> [File|H] end, []), 
    [gen_file_cache(F) || F <- ModulesAndHrls ++ IncludeHrls].

prepare_ignores() ->
    [ code:root_dir() ++ "/lib/" ++ atom_to_list(A) ++ "-" || A <- ?IGNORES].

create_cache_for_erlang_libs(CacheDir) ->
    create_cache(CacheDir, code:root_dir(), prepare_ignores()).

create_cache(CacheDir, File) ->
    generate_file(CacheDir, module_name(File), File, undefined),
    ok.

create_cache_file_fly(FlyFile, RealFile) ->
    CacheDir = eide_connect:prop(cache_dir) ++ "/" ++ eide_connect:prop(project_name),
    RealModuleName = module_name(RealFile),    
    ModuleName = module_name(FlyFile),    
    CacheFileName = get_cache_file_name(CacheDir, RealFile, RealModuleName),
    try 
       Data = generate(ModuleName, FlyFile, RealFile, undefined, RealModuleName), 
       dump_data_to_file(RealModuleName, CacheDir, RealFile, CacheFileName, Data, FlyFile),
       send_answer(CacheFileName, RealFile)
    catch _:_ -> ok
    end,    
    ok.

create_cache(CacheDir, AppsPath, Ignores) ->
    {ok, Re} = re:compile("(^.*?/(src|include)/.*\.(erl|hrl)$|^.*?/html/.*\.html$)"),
    IsInIgnoreFun = fun(File) -> lists:any(fun(IApp) -> lists:prefix(IApp, File) end, Ignores) end,
    IsInWrongFolder = fun(File) -> re:run(File, Re) == nomatch end,
    Files = filelib:fold_files(AppsPath, ".*\.(erl|hrl|html)$", true, 
                               fun(File, A) ->
                                   case IsInIgnoreFun(File) orelse IsInWrongFolder(File) of
                                       true -> 
                                           A;
                                       _ -> 
                                           Ext = filename:extension(File),
                                           ModuleName = module_name(File),
                                           case Ext of
                                               ".erl" ->
                                                   case dict:find(ModuleName, A) of
                                                       {ok, {_, D}} ->
                                                           dict:store(ModuleName, {File, D}, A);
                                                       error ->
                                                           dict:store(ModuleName, {File, undefined}, A)
                                                   end;
                                               ".hrl" -> 
                                                   dict:store(ModuleName, {File, undefined}, A);
                                               ".html" ->
                                                   case dict:find(ModuleName, A) of
                                                       {ok, {F, undefined}} ->
                                                           dict:store(ModuleName, {F, File}, A);
                                                       {ok, {_, _}} ->
                                                           A;
                                                       error ->
                                                           dict:store(ModuleName, {undefined, File}, A)
                                                   end;
                                               _ -> A
                                           end
                                   end 
                               end, dict:new()),
    [generate_file(CacheDir, Name, File, Docs) || {Name, {File, Docs}} <- dict:to_list(Files), File =/= undefined].

module_name(File) ->
    Ext = filename:extension(File),
    ModuleName = filename:rootname(filename:basename(File)),
    case Ext of
        ".hrl" -> ModuleName ++ ".hrl";
        _ -> ModuleName
    end.

get_cache_file_name(CacheDir, FilePath, Name) ->
    CacheDir ++ "/" ++ eide_compiler:app_name(FilePath) ++ "/" ++ Name ++ ".cache".

generate_file(CacheDir, ModuleName, FilePath, DocsFilePath) ->
    try
        CacheFileName = get_cache_file_name(CacheDir, FilePath, ModuleName),
%        case filelib:last_modified(FilePath) < filelib:last_modified(CacheFileName) of
%            true -> ignore;
%            _ ->
        Data = generate(ModuleName, FilePath, FilePath, DocsFilePath, ModuleName),
        dump_data_to_file(ModuleName, CacheDir, FilePath, CacheFileName, Data),
%        end,
        send_answer(CacheFileName, FilePath) 
    catch 
        exit:{ucs, {bad_utf8_character_code}} ->
            ok;
        Error:Reason:Stacktrace ->
            io:format("File:~p Error:~p, ~p~n~p~n", [FilePath, Error, Reason, Stacktrace])
    end.

send_answer(CacheFile, File) -> 
    Response = mochijson2:encode({struct, [
        {response, gen_file_cache},   
        {path, iolist_to_binary(File)},     
        {cache_path, iolist_to_binary(CacheFile)}]
    }), 
    eide_connect:send(Response).

dump_data_to_file(ModuleName, CacheDir, FilePath, CFile, Content) ->
    dump_data_to_file(ModuleName, CacheDir, FilePath, CFile, Content, FilePath).

dump_data_to_file(ModuleName, CacheDir, FilePath, CFile, Content, FlyFileName) ->
    #content{module_name = MN, 
             functions = Funs, 
             records = Recs, 
             macros = Macs, 
             includes = Incs,
             callbacks = Callbacks,
             exported_types = ExpTypes,
             behaviors = Behaviors} = Content, 
    Props = [ 
        {name, list_to_binary(ModuleName)},
        {file, list_to_binary(FilePath)},
        {funs, funs_to_json(MN, CacheDir, Funs, FilePath)},
        {macros, {struct, macros_to_json(Macs, FlyFileName)}},
        {callbacks, {struct, callbacks_to_json(Callbacks)}},
        {includes, includes_to_json(sets:to_list(sets:from_list(Incs)), FlyFileName)},
        {records_data, {struct, recs_data_to_json(Recs, FlyFileName)}},
        {exported_types, {struct, exp_types_to_json(ExpTypes)}},
        {behaviors, [atom_to_binary(B, latin1) || B <- Behaviors]}
    ], 
    Dirname = filename:basename(filename:dirname(FilePath)),
    Extension = filename:extension(FilePath),
    Props1 =  
        case {Extension, Dirname} of
            {".hrl", Dirname} ->
                [{is_global_include, Dirname == "include" }|Props]; 
             _ ->
                Props
        end,
    JsonStruct =  {struct, Props1},
    catch file:make_dir(filename:dirname(CFile)),
    write_json(CFile, JsonStruct).

write_json(File, Json) ->
    StringData = iolist_to_binary(lists:flatten(mochijson2:encode(Json))),
    file:write_file(File, StringData).
 
funs_to_json(ModuleName, CacheDir, Funs, FilePath) -> 
    lists:reverse([fun_to_json(ModuleName, CacheDir, F, FilePath) || F <- Funs]).

fun_to_json(ModuleName, CacheDir, Fun, FilePath) ->
    {Name, Arity} = Fun#function.name,
    DocRef =  
        case Fun#function.doc of
            undefined -> [];
            Text ->
                FileName = ModuleName ++ "_" ++ Name ++ "-" ++ integer_to_list(Arity) ++ ".fun",
                FullPath = CacheDir ++ "/" ++ eide_compiler:app_name(FilePath) ++ "/" ++ FileName,
                file:write_file(FullPath, iolist_to_binary(Text)),
                FullPath
        end, 
    Line = case Fun#function.line of {L, _} -> L; L -> L end,
    Data =    
    [ 
     {name, iolist_to_binary(Name)},
     {arity, Arity},
     {line, Line},
     {exported, Fun#function.exported},
     {params, [lists:map(fun iolist_to_binary/1, P) || P <- Fun#function.params]},
     {types, [lists:map(fun iolist_to_binary/1, P) || P <- Fun#function.types]},
     {result, [iolist_to_binary(P) || P <- Fun#function.result]}
    ],
    Data1 = 
        case Fun#function.comment of 
            [] -> Data;
            _ ->Data ++ [{comment, iolist_to_binary(Fun#function.comment)}]
        end,
    Data2 = 
        case DocRef of 
            [] -> Data1;
            _ ->Data1 ++ [{docref, iolist_to_binary(DocRef)}]
        end,
    Data2 ++ [{bif, ModuleName == "erlang" orelse Fun#function.bif}].

recs_data_to_json(Recs, File) ->
    [rec_data_to_json(R) || R <- Recs, string:to_lower(R#record.file) == string:to_lower(File)].

exp_types_to_json(Recs) ->
    [{list_to_binary(TypeName), {struct, [{types, list_to_binary(Types)}, {line, Line}]}}|| {TypeName, Types, Line} <- Recs].
 
rec_data_to_json(Rec) ->
    Result = {Rec#record.name, {struct, [{fields, rec_fields_to_json(Rec#record.fields)}, 
                                {types, rec_field_types_to_json(Rec#record.fields)},
                                {line, Rec#record.line}]}
    },
%    erlang:display(Result),
    Result.

macros_to_json(Macs, File) ->
    [macros_data_to_json(M) || M <- Macs, string:to_lower(M#macro.file) == string:to_lower(File)].

macros_data_to_json(Mac) ->
    {Mac#macro.name, {struct, [{value, iolist_to_binary(Mac#macro.value)}, 
                                {line, Mac#macro.line}]}
    }.

callbacks_to_json(Callbacks) ->
    [begin
        {atom_to_binary(Fun, latin1), {struct, [{params, [to_binary(P) || P <- Params]}]}} 
    end|| #callback{function = Fun, line = _Line, params = Params} <- Callbacks].

to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, latin1);
to_binary(List) when is_list(List) ->
    iolist_to_binary(List);
to_binary(Bin) when is_binary(Bin) ->
    Bin;
to_binary(Int) when is_integer(Int) ->
    integer_to_binary(Int);
to_binary(Float) when is_float(Float) ->
    float_to_binary(Float).

rec_fields_to_json(Fields) -> 
    [list_to_binary(F) || #field{name = F} <- Fields].
 
rec_field_types_to_json(Fields) ->
    [list_to_binary(T) || #field{type = T} <- Fields].
 
includes_to_json(Incs, File) ->
    [begin
        I1 = case lists:prefix("../", I) of
            true -> filename:absname(I, filename:dirname(File));
            _ -> I
        end,   
        [iolist_to_binary(eide_compiler:app_name(I1)), iolist_to_binary(filename:basename(I))]
    end
    || I <- Incs, string:to_lower(I) =/= string:to_lower(File)].

generate(ModuleName, FilePath, RealFile, DocsFilePath, RealModuleName) -> 
    {StartContent, SyntaxTree} = 
        case filename:extension(FilePath) of
            ".erl" -> 
                generate_from_source(FilePath, RealFile);
            ".hrl" -> 
                generate_from_source(FilePath, RealFile);
            _ -> 
                throw(unknown_type) 
        end,
    Comments = erl_comment_scan:file(FilePath),
    {SyntaxTree1, _} = erl_recomment:recomment_tree(SyntaxTree, Comments),
   % io:format("Syntax tree: ~p", [SyntaxTree1]),
    Content = parse_tree(SyntaxTree1, StartContent),  
    Incls = sets:to_list(sets:from_list(Content#content.includes)),
    BeamTree = get_tree_from_beam(RealModuleName), 
    Content1 = case BeamTree of
        undefined -> Content;
        _ -> parse_beam_tree(BeamTree, Content)
    end,
    Content2 = case {DocsFilePath, filename:extension(FilePath), RealModuleName == ModuleName} of
%                   {undefined, ".erl", true} ->
%                       merge_with_edoc(Content, FilePath);
                   {File, ".erl", true} when File =/= undefined -> 
                       merge_with_docs_file(Content1, DocsFilePath);
                   _ ->
                       Content1
               end,
    Content2#content{includes = Incls, file = FilePath, module_name = ModuleName}.

generate_from_source(Path, RealPath) ->   
    {ok, Source} = epp:parse_file(Path, [ eide_compiler:app_path(RealPath) ++ "/include"], []), 
    {ok, SourceMacros}  = epp_dodger:parse_file(Path),
    Content = parse_tree_simple(erl_syntax:form_list(SourceMacros), #content{file = Path, last_file_attr = Path}),
    {#content{  
            file = Path,  
            last_file_attr = Path, 
            macros = Content#content.macros,    
            includes = Content#content.includes
        }, 
        erl_syntax:form_list(Source)}.
 
    
get_tree_from_beam(ModuleName) ->
    case code:where_is_file(ModuleName ++ ".beam") of
        non_existing -> 
            undefined;
        BeamPath -> 
            case beam_lib:chunks(BeamPath, [abstract_code]) of
                {ok, {_,[{abstract_code,{_, AbstractCode}}]}} -> 
                    erl_syntax:form_list(AbstractCode);
                _ -> 
                    undefined
            end
    end.
    
%generate_from_beam(ModuleName, Beam) ->
    

parse_tree_simple(Node, Content) ->
    case erl_syntax:type(Node) of
        form_list -> 
            lists:foldl(fun parse_tree_simple/2, Content,
                        erl_syntax:form_list_elements(Node));
        
        attribute ->
            Name = erl_syntax:attribute_name(Node),
            case erl_syntax:type(Name) of
                atom ->
                    parse_atom_simple(Node, erl_syntax:atom_literal(Name), Content);
                _ ->
                    Content
            end;
        _ ->
            Content
    end.

parse_beam_tree(Node, Content) ->
    case erl_syntax:type(Node) of
        form_list -> 
            lists:foldl(fun parse_beam_tree/2, Content,
                        erl_syntax:form_list_elements(Node));
        attribute ->
            Name = erl_syntax:attribute_name(Node),
            case erl_syntax:type(Name) of 
                atom ->
                    parse_atom_beam(Node, erl_syntax:atom_literal(Name), Content);
                _ ->
                    Content
            end;
        function ->
            NameInfo = erl_syntax:function_name(Node),
            Name = erl_syntax:atom_literal(NameInfo),
            Arity = erl_syntax:function_arity(Node), 
            %io:format("~p. find result:~p~n", [{Name, Arity}, lists:keyfind({Name, Arity}, #function.name, Content#content.functions)]), 
            case lists:keyfind({Name, Arity}, #function.name, Content#content.functions) of
                false ->
                    Function = parse_erlang_function(Node, Content, erl_syntax:get_precomments(Node)),
                    Exported = 
                        case Content#content.exports of
                            all -> true;
                            _ -> lists:member({Name, Arity}, Content#content.exports)
                        end,
                    Functions = [Function#function{exported = Exported}|Content#content.functions],
                    Content#content{functions = Functions};
                _ ->
                    Content
            end;
        _ ->
            Content
    end.

parse_tree(Node, Content) -> 
    case erl_syntax:type(Node) of
        form_list -> 
            lists:foldl(fun parse_tree/2, Content,
                        erl_syntax:form_list_elements(Node));
        
        attribute ->
            Name = erl_syntax:attribute_name(Node),
            case erl_syntax:type(Name) of
                atom ->
                    parse_atom(Node, erl_syntax:atom_literal(Name), Content);
                _ ->
                    Content
            end;
         
        function ->
            Function = parse_erlang_function(Node, Content, erl_syntax:get_precomments(Node)),
            NameInfo = erl_syntax:function_name(Node),
            Name = erl_syntax:atom_literal(NameInfo),
            Arity = erl_syntax:function_arity(Node),
            Exported = 
                case Content#content.exports of
                    all -> true;
                    _ -> lists:member({Name, Arity}, Content#content.exports)
                end,
            Functions = [Function#function{exported = Exported}|Content#content.functions],
            Content#content{functions = Functions};
        
        comment -> Content;
        _ ->
            Content
    end.

parse_atom(Node, "file", Content) ->
    [Attribute, _] = erl_syntax:attribute_arguments(Node),
    File = erl_syntax:string_value(Attribute),
    Content1 =     
        case lists:suffix("hrl", File) andalso File =/= Content#content.file of
            true -> 
                Content#content{includes = [File | Content#content.includes]};
            false -> 
                Content
        end,
    Content1#content{last_file_attr = File}; 
parse_atom(Node, "export", Content) -> 
    case Content#content.exports of
        all ->
            Content;
        _ ->
            [ExportAttrs] = erl_syntax:attribute_arguments(Node),
            Exports = lists:map(fun (Export) ->
                                   parse_export(Export)
                                end,
                      erl_syntax:list_elements(ExportAttrs)),
            Content#content{exports = Content#content.exports ++ Exports}
    end;
parse_atom(Node, "module", Content) -> 
    [Attribute|_] = erl_syntax:attribute_arguments(Node),
    ModuleName = erl_syntax:atom_value(Attribute),
    case is_list(ModuleName) of
        true ->
            throw(skip_module);
        _ ->
            Content#content{module_name = ModuleName}
    end;
parse_atom(Node, Atom, Content) when Atom == "behavior" orelse Atom == "behaviour" -> 
    [Attribute|_] = erl_syntax:attribute_arguments(Node),
    ModuleName = erl_syntax:atom_value(Attribute),
    Content#content{behaviors = [ModuleName | Content#content.behaviors]};
parse_atom(Node, "compile", Content) -> 
    [Attribute] = erl_syntax:attribute_arguments(Node),
    case erl_syntax:atom_value(Attribute) of
        export_all ->
            Content#content{exports = all};
        _ ->
            Content
    end; 
parse_atom(Node, "record", Content) -> 
    Record = parse_erlang_record(Node, Content),
    Records = [Record#record{file = Content#content.last_file_attr} | Content#content.records],
    Content#content{records = Records};
parse_atom(Node, "spec", Content) -> 
    Spec = parse_spec(Node),
    case Spec of 
        none -> Content;
        _ -> Content#content{specs = [Spec | Content#content.specs]}
    end; 
parse_atom(Node, "callback", Content) -> 
    Callback = parse_callback(Node),
    case Callback of 
        none -> Content;
        _ -> Content#content{callbacks = [Callback | Content#content.callbacks]}
    end; 
parse_atom(Node, "type", Content) -> 
    parse_types(Node, Content);
parse_atom(Node, "opaque", Content) -> 
    parse_opaque(Node, Content);
parse_atom(_Node, _Atom, Content) -> 
    Content.

parse_atom_simple(Node, "define", Content) -> 
    try
        Macro = parse_erlang_macro(Node),
        Macros = [Macro#macro{file = Content#content.last_file_attr} | Content#content.macros],
        Content#content{macros = Macros}
    catch _:_ ->
        Content
    end;
parse_atom_simple(_, _, Content) ->
    Content.
    
parse_atom_beam(Node, "export", Content) -> 
    case Content#content.exports of
        all ->
            Content;
        _ ->
            [ExportAttrs] = erl_syntax:attribute_arguments(Node),
            Exports = lists:map(fun (Export) ->
                                   parse_export(Export)
                                end,
                      erl_syntax:list_elements(ExportAttrs)),
            Content#content{exports = Content#content.exports ++ Exports}
    end;
parse_atom_beam(_, _, Content) ->
    Content.
    
parse_export(Export) -> 
    {erl_syntax:atom_literal(erl_syntax:arity_qualifier_body(Export)),
     erl_syntax:integer_value(erl_syntax:arity_qualifier_argument(Export))}.  

parse_erlang_function(Node, Content, Comments) ->
    NameInfo = erl_syntax:function_name(Node),
    Name = erl_syntax:atom_literal(NameInfo),
    Line = erl_syntax:get_pos(NameInfo),
    Clauses = erl_syntax:function_clauses(Node),
    Arity = erl_syntax:function_arity(Node),
    Params = 
        [[begin
             Var = node_value(P, erl_syntax:type(P)),
             Var
         end || P <- erl_syntax:clause_patterns(Clause)] || Clause <- Clauses],

    Spec = case lists:keyfind({Name, Arity}, #spec.name, Content#content.specs) of 
               false -> undefined;
               S -> S#spec{name = element(1, S#spec.name)}
           end,  
    Params1 = case Spec of
                  undefined -> Params;
                  Spec -> 
                      Params0 = case length(Params) < length(Spec#spec.vars) of 
                          true -> lists:duplicate(length(Spec#spec.vars), hd(Params));
                          _ -> Params
                      end,
                      [[begin
                          V = lists:nth(I, lists:nth(J, Spec#spec.vars)),
                          case V of
                              ?VAR -> lists:nth(I, lists:nth(J, Params0));
                              _ -> V
                          end
                      end|| I <- lists:seq(1, length(lists:nth(J, Spec#spec.vars)))] || J <- lists:seq(1, length(Spec#spec.vars))]
              end,
    Result =  
        case Spec of
            undefined -> lists:duplicate(length(Clauses), "term()");
            Spec -> Spec#spec.result
        end,
    Types = case Spec of
                undefined -> lists:duplicate(length(Clauses), []);
                _ -> Spec#spec.types
            end,
    Comment =
        case Comments of
            [] -> [];
            _ ->
                C = lists:last(Comments),
                erl_prettypr:format(C)
        end,

    #function{name = {Name, Arity},
              line = Line,
              params = Params1,
              types = Types,
              result = Result, 
              comment = Comment}.

node_value(Node, variable) ->
    erl_syntax:variable_literal(Node);
node_value(Node, atom) ->
    erl_syntax:atom_literal(Node);
    %"Atom";
node_value(Node, string) ->
    erl_syntax:string_literal(Node);
node_value(_Node, list) ->
    "List";
node_value(_Node, nil) ->
    "List";
node_value(_Node, tuple) ->
    "Tuple";
node_value(Node, record_expr) ->
    try 
        Record = atom_to_list(element(4, element(3, element(4, Node)))),
        string:to_upper(string:substr(Record, 1, 1)) ++ string:substr(Record, 2)
    catch _:_ -> "Record"
    end; 
node_value(Node, match_expr) ->
    try 
        erl_syntax:variable_literal(erl_syntax:match_expr_body(Node))
    catch _:_ -> ?VAR
    end;
node_value(_Node, _) ->
    ?VAR.

parse_spec(Node) ->
    try
        {spec, {_, Clauses}} = erl_syntax_lib:analyze_wild_attribute(Node),
        Specs = [parse_spec_internal(Node, C) || C <- lists:seq(1, length(Clauses))],
        {V, R, T} = lists:unzip3(lists:map(fun(#spec{vars = V, result = R, types = T}) -> {V, R, T} end, Specs)),
        Spec = (hd(Specs))#spec{vars = V, result = R, types = T},
        %io:format("Spec: ~p~n", [Spec]),
        Spec
    catch 
        Error:Reason:ST -> 
            io:format("~p", [{cant_parse_spec, Error, Reason, ST}]),
            none
    end.

parse_spec_internal(Node, Clause) ->
    %io:format("~p ~p~n", [Node, Clause]),
    Spec = edoc_specs:spec(Node),
    %io:format("~n SPEC: ~n~p~n~n~n", [Spec]),
    {_, _, _, _, TSpecs, _} = Spec,
    #t_spec{name = Name, type = Type, defs = Defs} = lists:nth(Clause, TSpecs),
    #t_name{name = FunName} = Name,
    #t_fun{args = Args, range = Range} = Type, 
    %io:format("~p~n", [Range]),
    {Vars, Types} = get_var_types(Args, Defs),
    Result = parse_t_type(Range, Defs),  
    #spec{name = {atom_to_list(FunName), length(Args)}, vars = Vars, result = Result, types = Types}.

parse_callback(Node) ->
    try
        {Fun, VarNames} = case erl_syntax_lib:analyze_wild_attribute(Node) of
            {callback, {{F, _Arity}, [{type, _, bounded_fun, [{type, _, 'fun', [{type, _, product, V} | _]} | _]}]}} -> 
                {F, vars(V)};
            {callback, {{F, _Arity}, [{type, _, 'fun', [{type, _, product, V} | _]} | _]}} -> 
                {F, vars(V)}
        end,
        #callback{function = Fun, params = VarNames, line = erl_syntax:get_pos(Node)}
    catch 
        Error:Reason:Stacktrace -> io:format("parse callback error: ~p~n", [{Error, Reason, Stacktrace}])
    end.

vars(Vars) -> [var(V) || V <- Vars].

get_var_types(Args, Defs) ->
    {Vars, Types} = 
        lists:foldl(
            fun(A, {Vars, Types}) ->
                %io:format("A:~p~n", [A]),
                T = get_type(A, Defs),
                V = get_var_name(A),
                %io:format("T:~p, V:~p~n", [T, V]),
                {[V| Vars], [T | Types]}
            end, 
            {[], []}, 
            Args),  
    %io:format("Args:~p~nVars: ~p~nTypes:~p~n", [Args, Vars, Types]),
    {lists:reverse(Vars), lists:reverse(Types)}.  

get_var_name(#t_nil{}) -> "[]";
get_var_name(#t_atom{val = Val}) -> atom_to_list(Val);
get_var_name(#t_integer{val = Val}) -> integer_to_list(Val);
get_var_name(#t_float{val = Val}) -> float_to_list(Val);
get_var_name(#t_name{name = Name}) -> 
    case is_atom(Name) of 
        true -> atom_to_list(Name);  
        _ -> ?VAR 
    end;
get_var_name(#t_type{a = Args})-> args_to_name(Args);
get_var_name(#t_list{})-> "List";
get_var_name(#t_map{})-> "Map";
get_var_name(#t_record{}) -> "Record";
get_var_name(#t_union{}) -> "Union";
get_var_name(#t_tuple{}) -> "Tuple";
get_var_name(#t_nonempty_list{}) -> "NonEmptyList";
get_var_name(#t_integer_range{}) -> "IntegerInRange";
get_var_name(#t_var{name = Name})-> atom_to_list(Name);
get_var_name(#t_fun{})-> "Function";
get_var_name(T)-> 
    io:format("Unknown var name: ~p~n", [T]).

get_type(T, Defs) -> parse_t_type(T, Defs).

parse_t_type(T, Deps) -> 
    parse_t_type(T, Deps, 6).
parse_t_type(_, _, 0) -> ?TERM;
parse_t_type(#t_nil{}, _, _) -> ?TERM;
parse_t_type(#t_type{name = Name}, _, _) when is_atom(Name) ->
    atom_to_list(Name) ++ "()"; 
parse_t_type(#t_type{name = Name}, Defs, Depth) when is_record(Name, t_name) ->
    parse_t_type(Name, Defs, Depth - 1); 
parse_t_type(#t_name{module = Module, name = Name}, _, _) ->
    Result = case Module of [] -> ""; _ -> atom_to_list(Module) ++ ":" end,
    Result ++ atom_to_list(Name) ++ "()";  
parse_t_type(#t_atom{val = Val}, _, _)->
    atom_to_list(Val); 
parse_t_type(#t_record{name = #t_atom{val = Name}}, _, _)->
    "#" ++ atom_to_list(Name) ++ "{}"; 
parse_t_type(#t_union{types = Types}, Defs, Depth) -> 
    "(" ++ string_join([parse_t_type(T, Defs, Depth - 1) || T <- Types], " | ") ++ ")";
parse_t_type(#t_tuple{types = Types}, Defs, Depth) ->
    "{" ++ string_join([parse_t_type(T, Defs, Depth - 1) || T <- Types], ", ") ++ "}";
parse_t_type(#t_list{type = Type}, Defs, Depth) ->
    "[" ++ parse_t_type(Type, Defs, Depth - 1) ++ "]";
parse_t_type(#t_nonempty_list{type = Type}, Defs, Depth) ->
    "[" ++ parse_t_type(Type, Defs, Depth - 1) ++ "] (Non empty)";
parse_t_type(#t_integer_range{from = From, to = To}, _Defs, _Depth) ->
    "integer(" ++ integer_to_list(From) ++ ".." ++ integer_to_list(To) ++ ")";
parse_t_type(#t_var{name = Names}, Defs, Depth) ->
    Name = case Names of
               Names when is_list(Names) -> hd(Names);
               _ -> Names
           end,
    case lists:keyfind(#t_var{name = Name}, #t_def.name, Defs) of
        #t_def{type = T} -> 
            parse_t_type(T, Defs, Depth - 1);
        _ -> ?TERM 
    end;
parse_t_type(#t_map{types = Types}, Defs, Depth) ->
    "#{" ++ string_join([parse_t_type(T, Defs, Depth - 1) || T <- Types], ", ") ++ "}";
parse_t_type(#t_fun{args = Args, range = Range}, Defs, Depth) ->
    "fun(" ++ string_join([parse_t_type(A, Defs, Depth - 1) || A <- Args], ", ") ++ ") -> " ++ parse_t_type(Range, Defs, Depth - 1);
parse_t_type(#t_map_field{k_type = KType, v_type = VType}, Defs, Depth) ->
    parse_t_type(KType, Defs, Depth - 1) ++ " => " ++ parse_t_type(VType, Defs, Depth - 1);
parse_t_type(#t_paren{type = Type}, Defs, Depth) -> 
    "(" ++ parse_t_type(Type, Defs, Depth - 1) ++ ")";
parse_t_type(#t_binary{base_size = BaseSize, unit_size = UnitSize}, _Defs, _Depth) -> "binary(Base size: " ++ integer_to_list(BaseSize) ++", Unit size: " ++ integer_to_list(UnitSize) ++ ")"; 
parse_t_type(#t_integer{val = Val}, _Defs, _Depth) -> "integer(" ++ integer_to_list(Val) ++ ")"; 
parse_t_type(TType, _, _) -> 
    io:format("Unknown field type: ~p~n~p", [TType, erlang:process_info(self(), current_stacktrace)]),
    ?TERM.

string_join(Items, Sep) ->
    lists:flatten(lists:reverse(string_join1(Items, Sep, []))).
string_join1([], _Sep, Acc) ->  
    Acc;
string_join1([Head | []], _Sep, Acc) ->
    [Head | Acc];
string_join1([Head | Tail], Sep, Acc) ->
    string_join1(Tail, Sep, [Sep, Head | Acc]).

args_to_name([]) -> ?VAR;
args_to_name([{type_variables, [V]}]) -> atom_to_list(V);
args_to_name(Args) -> atom_to_list(hd(Args)).

parse_erlang_record(Node, _Content) ->
%    io:format("rec: ~p~n", [erl_syntax_lib:analyze_record_attribute(Node)]),
    {RecordName, Fields} = erl_syntax_lib:analyze_record_attribute(Node),
     %= [_, {F, {_, TA}}, {F2, {_, TA2}}, _]}
%     erlang:display([parse_erlang_record_field(F) || F <- Fields]),
    #record{
        line = erl_syntax:get_pos(Node),
        name = atom_to_list(RecordName),
        fields = [parse_erlang_record_field(F) || F <- Fields]
    }.

parse_erlang_record_field(Field) ->
    case Field of
        {FieldName, {_Default, Type}} ->
            #field{name = atom_to_list(FieldName), type = parse_erlang_record_field_type(Type)};
        {FieldName, _Default} -> 
            #field{name = atom_to_list(FieldName), type = "undefined"}
    end.

parse_erlang_record_field_type(none) -> "undefined";
parse_erlang_record_field_type({var, _, V}) -> 
    case is_atom(V) of
        true -> atom_to_list(V);
        _ -> term_to_binary(V)
    end;
parse_erlang_record_field_type({type, _, 'fun', []}) -> atom_to_list('fun') ++ "()";
parse_erlang_record_field_type({type, _, map, _}) -> "#{}";
parse_erlang_record_field_type({type, _, tuple, _}) -> "{}";
parse_erlang_record_field_type({tree, integer_range_type, _, {integer_range_type, I1, I2}}) -> parse_erlang_record_field_type(I1) ++ ".." ++ parse_erlang_record_field_type(I2);
parse_erlang_record_field_type({integer, _, Int}) -> integer_to_list(Int);
parse_erlang_record_field_type({tree, prefix_expr, _, {prefix_expr, {tree, operator, _, Op}, T}}) -> atom_to_list(Op) ++ parse_erlang_record_field_type(T);
%parse_erlang_record_field_type({type, record_type, _, RT}) -> parse_erlang_record_field_type(RT);
parse_erlang_record_field_type({tree, map_type, _, MapAssocs}) -> "#{" ++ string_join([parse_map_assoc(MapAssoc) || MapAssoc <- MapAssocs], ", ") ++ "}"; 
parse_erlang_record_field_type({tree, record_type, _, RT}) -> parse_erlang_record_field_type(RT);
parse_erlang_record_field_type({record_type, {atom, _, Atom}, _}) -> "#" ++ atom_to_list(Atom) ++ "{}";
parse_erlang_record_field_type({wrapper, _, _, Atom}) -> parse_erlang_record_field_type(Atom);
parse_erlang_record_field_type({atom, _, Atom}) -> atom_to_list(Atom);
parse_erlang_record_field_type({tree, annotated_type, _, {annotated_type, Var, T}}) -> var(Var) ++ " :: " ++ parse_erlang_record_field_type(T);
parse_erlang_record_field_type({tree, function_type, _, FT}) -> parse_erlang_record_field_type(FT);
parse_erlang_record_field_type({function_type, Vars, RType}) -> 
    "fun(" ++ string_join([var(V) || V <- Vars], ", ") ++ ") -> " ++ parse_erlang_record_field_type(RType);
parse_erlang_record_field_type({tree, type_union, _, TypeList}) ->
    Types = lists:foldl(fun(T, Acc) -> [parse_erlang_record_field_type(T) | Acc] end, [], TypeList),
    string_join(lists:reverse(Types), " | "); 
parse_erlang_record_field_type({tree, tuple_type, _, TypeList}) ->
    Types = lists:foldl(fun(T, Acc) -> [parse_erlang_record_field_type(T) | Acc] end, [], TypeList),
    string_join(lists:reverse(Types), " | ");
parse_erlang_record_field_type(Type) ->
    try
        case erl_syntax_lib:analyze_type_application(Type) of
            {M, {F, _A}} -> atom_to_list(M) ++ "/" ++ atom_to_list(F) ++ "()";
            {T, _} -> atom_to_list(T) ++ "()";
            Unknown ->
                io:format("Unknown field application type: ~p~n", [Unknown]),
                "FIX IT"
        end
    catch E:R:Stacktrace ->
        io:format("Unknown field type: ~p~n~p~n", [Type, Stacktrace]),
        "FIX IT"
    end.

var({var, _, Var}) -> atom_to_list(Var);
var({ann_type, _, [{var, _, Var}, _]}) ->
    atom_to_list(Var);
var({atom, _, Atom}) ->
    atom_to_list(Atom) ++ "()";
var({user_type, _, Atom, _}) ->
    atom_to_list(Atom) ++ "()";
var({type, _, 'fun', _}) -> "fun()";
var({type, _, term, _}) -> "term()";
var({type, _, binary, _}) -> "binary()";
var({type, _, string, _}) -> "string()";
var({type, _, record, [{atom, _, Var}]}) ->
    atom_to_list(Var);
var({tree, integer_range_type, _, {integer_range_type, I1, I2}}) ->
    var(I1) ++ ".." ++ var(I2);
var({remote_type, _, record, [{atom, _, M}, {atom, _, F}, _]}) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "()";
var({remote_type, _, [{atom, _, M}, {atom, _, F}, _]}) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "()";
var({type, _, tuple, Vars}) ->
    "{" ++ string_join(vars(Vars), ", ") ++ "}";
var({type, _, union, Vars}) ->
    string_join(vars(Vars), " | ");
var({type, _, list, Vars}) ->
    string_join(vars(Vars), " | ");
var({tree, type_application, _, _} = T) ->
    parse_erlang_record_field_type(T);
var(V) ->
    io:format("Unknown var: ~p~n~p", [V, erlang:process_info(self(), current_stacktrace)]),
    "term()".


parse_map_assoc({tree,map_type_assoc, _, {map_type_assoc, T1, T2}}) -> 
    parse_erlang_record_field_type(T1) ++ " => " ++ parse_erlang_record_field_type(T2);
parse_map_assoc({tree,map_type_exact, _, {map_type_exact, T1, T2}}) -> 
    parse_erlang_record_field_type(T1) ++ " => " ++ parse_erlang_record_field_type(T2);
parse_map_assoc(MA) -> 
    io:format("Unknown map assoc record type: ~p~n~p", [MA, erlang:process_info(self(), current_stacktrace)]),
    "FIX IT".

parse_opaque(Node, Content) -> 
    [Args|_] = erl_syntax:attribute_arguments(Node), 
    [Name, Type | _] = erl_syntax:tuple_elements(Args),
    ExportedTypes = {atom_to_list(element(4,Name)), parse_types(Type), erl_syntax:get_pos(Node)},
    Content#content{exported_types = [ExportedTypes | Content#content.exported_types]}.

parse_types(Node, Content) ->
    try
        [Args|_] = erl_syntax:attribute_arguments(Node),
        
        TE = erl_syntax:tuple_elements(Args),
        [TypeData, Data| _] = TE,
        case TypeData of 
            TypeData when element(2, TypeData) == tuple ->
                [_, RecordNameNode] = erl_syntax:tuple_elements(TypeData),
                RecordName = erl_syntax:atom_name(RecordNameNode),
                
                FieldTypes = lists:foldl(fun(E, Acc) -> [get_field_type(E)| Acc] end, [], erl_syntax:list_elements(Data)),
                Records = Content#content.records,
                Record = lists:keyfind(RecordName, #record.name, Records),
                Fields = Record#record.fields,
                NewFields = lists:map(
                    fun(Field) ->
                        #field{name = Field#field.name, type = proplists:get_value(Field#field.name, FieldTypes, Field#field.type)}
                    end, Fields),
                Record1 = Record#record{fields = NewFields},
                Content1 = Content#content{records = lists:keystore(RecordName, #record.name, Records, Record1)},
                Content1;
            TypeData when element(2, TypeData) == atom ->
                ExportedTypes = {atom_to_list(element(4,TypeData)), parse_types(Data), erl_syntax:get_pos(Node)},
                Content#content{exported_types = [ExportedTypes | Content#content.exported_types]};
            _ ->
                Content
        end
    catch _Error:_Reason ->
        Content
    end.

get_field_type(TypeData) ->
    {tree,tuple,
        {attr,0,[],none},
        [{tree,atom,{attr,0,[],none},TypeField}|Data]} = TypeData,
    case TypeField of 
        typed_record_field -> 
            [RecordNameData, FieldData] = Data,
            {get_record_field_name(RecordNameData), parse_types(FieldData)};
        _ -> 
            {get_record_field_name(TypeData), "undefined"}
    end.

get_record_field_name(Data) ->
    {tree,tuple,
       {attr,0,[],none},
       [{tree,atom,{attr,0,[],none},record_field},
        _,
        {tree,tuple, 
          {attr,0,[],none},
          [{tree,atom,{attr,0,[],none},atom}, 
           _, 
           {tree,atom,{attr,0,[],none},FieldName}]}|_]} = Data,
    atom_to_list(FieldName).

parse_types(TData) ->
    {tree,tuple,  
       {attr,0,[],none}, 
       [{tree,atom,{attr,0,[],none},TType},
        {tree,integer,{attr,0,[],none},_}|Data]} = TData,
    case TType of
        remote_type ->
            parse_remote_type(Data);
        ann_type ->
            parse_ann_type(Data);
        integer ->
            [{tree,integer,{attr,0,[],none},Int}] = Data,
            integer_to_list(Int);
        paren_type ->
            [{tree,list,{attr,0,[],none},{list,TypeData,none}}] = Data,
            Types = lists:foldl(fun(T, Acc) -> [parse_types(T) | Acc] end, [], TypeData),
                "[ " ++ string_join(lists:reverse(Types), " , ") ++ " ]"; 
        _ ->
            [{tree,atom,{attr,0,[],none},Type} | UnionData] = Data,
            case Type of
                record ->
                    [{tree,list,{attr,0,[],none},{list,TypeData,none}}] = UnionData,
                    T = parse_types(hd(TypeData)),
                    "#" ++ (T -- "()") ++ "{}";
                union -> 
                    [{tree,list,{attr,0,[],none},{list,TypeData,none}}] = UnionData,
                    Types = lists:foldl(fun(T, Acc) -> [parse_types(T) | Acc] end, [], TypeData),
                    string_join(lists:reverse(Types), " | "); 
                tuple when element(2, hd(UnionData)) == list ->
                    [{tree,list,{attr,0,[],none},{list,TypeData,none}}] = UnionData,
                    Types = lists:foldl(fun(T, Acc) -> [parse_types(T) | Acc] end, [], TypeData),
                    "{ " ++ string_join(lists:reverse(Types), " , ") ++ " }"; 
                _ when TType == atom ->
                    "'" ++ atom_to_list(Type) ++ "'";
                _ when TType == var ->
                    atom_to_list(Type);
                _ ->
                    case Type of 
                        undefined -> "undefined";
                        _ -> atom_to_list(Type) ++ "()"
                    end
            end
    end.

parse_ann_type(Data) ->
    [{tree,list,
      {attr,0,[],none},
      {list,
       [{tree,tuple,
         {attr,0,[],none},
         [{tree,atom,{attr,0,[],none},var},
          _,
          {tree,atom,{attr,0,[],none}, VarName}]},
        TypeData],
       _}}] = Data,
    atom_to_list(VarName) ++ " :: " ++ parse_types(TypeData) ++ "()".

parse_remote_type(Data) ->
    [{tree,list,
    {attr,0,[],none},
    {list,
     [{tree,tuple,
       {attr,0,[],none},
       [{tree,atom,{attr,0,[],none},atom},
        _,
        {tree,atom,{attr,0,[],none},Module}]},
      {tree,tuple,
       {attr,0,[],none},
       [{tree,atom,{attr,0,[],none},atom},
        _,
        {tree,atom,{attr,0,[],none},Type}]},
      _],
     none}}] = Data,
     atom_to_list(Module) ++ ":" ++ atom_to_list(Type) ++ "()".

parse_erlang_macro(Node) -> 
    [Define | Definition] = erl_syntax:attribute_arguments(Node),
    [Def| _] = Definition,
    Macro = erl_prettypr:format(Define),
    Value = erl_prettypr:format(Def),
    #macro{name = Macro, value = Value, line = erl_syntax:get_pos(Node)}.

%merge_with_edoc(Content, FilePath) ->
%    try
%        Docs = lists:flatten(edoc:read(FilePath)),
%        Functions = get_functions_data_from_edoc(Docs),
%        lists:foldl(fun add_data_from_edoc_fun/2, Content, Functions)
%    catch E:R ->
%        io:format("merge with docs error:~p~n", [{E, R}]),
%        Content
%    end.
    

merge_with_docs_file(Content, DocsFilePath) ->
    try
        {ok, Docs} = file:read_file(DocsFilePath),
        Functions = get_functions_data_from_html(Docs),
        lists:foldl(fun add_data_from_html_fun/2, Content, Functions)
    catch E:R ->
        io:format("merge with docs file error:~p~n", [{E, R}]),
        Content 
    end. 

get_functions_data_from_html(Data) ->
    Result = re:run(Data, eide_connect:prop(doc_re), [global, {capture, [1], list}]),
    PartDocRe = eide_connect:prop(part_doc_re),
    case Result of
        {match, Captured} ->
            FunsData = 
                [begin
                     FunsResult = re:run(Text, PartDocRe, [global, {capture, [1, 2, 3, 4, 5], list}]),
                     case FunsResult of
                         {match, Funs} ->
                             [{F, Text} || F <- Funs];
                         _ -> []
                     end
                 end || [Text] <- Captured],
            lists:append(FunsData);
        _ -> []
    end.

add_data_from_html_fun({[FunName, Arity, SpecName, Params, _Result], Text}, Content) -> 
    Arity1 = list_to_integer(Arity),
    SimpleParams = re:run(Params, eide_connect:prop(simple_param_re)),
    case lists:keyfind({FunName, Arity1}, #function.name, Content#content.functions) of
        Fun when is_record(Fun, function) -> 
            NewFun = 
                case SimpleParams of
                    true -> Fun#function{params = [[Params]], result = [?TERM], doc = Text};
                    _ -> Fun#function{doc = Text}
                end, 
            case FunName of
                "atom_to_list" -> erlang:display({add_data_from_html_fun, SpecName});
                _ -> ignore
            end,
            IsBif = case SpecName of "erlang:" ++ _ -> false; _ -> true end,
            NewFun1 = NewFun#function{bif = IsBif},
            Content#content{functions = lists:keyreplace({FunName, Arity1}, #function.name, Content#content.functions, NewFun1)};
        _ -> 
            case FunName of
                "atom_to_list" -> erlang:display({add_data_from_html_fun_2, SpecName});
                _ -> ignore
            end,
            IsBif = case SpecName of "erlang:" ++ _ -> false; _ -> true end,
            NewFun = #function{name = {FunName, Arity1}, params = [[Params]], result = [?TERM], doc = Text, bif = IsBif, exported = true},
            Content#content{functions = [NewFun|Content#content.functions]}
    end.

%get_functions_data_from_edoc(Data) ->
%    Result = re:run(Data, eide_connect:prop(edoc_re), [global, {capture, [1], list}]),
%    PartDocRe = eide_connect:prop(part_edoc_re),
%    case Result of
%        {match, Captured} ->
%            FunsData =
%                [begin
%                     FunsResult = re:run(Text, PartDocRe, [global, {capture, [1, 2], list}]),
%                     case FunsResult of
%                         {match, Funs} ->
%                             [{F, Text} || F <- Funs];
%                         _ -> []
%                     end
%                 end || [Text] <- Captured],
%            lists:append(FunsData);
%        _ -> []
%    end.

%add_data_from_edoc_fun({[FunName, Arity], Text}, Content) ->
%    Arity1 = list_to_integer(Arity),
%    case lists:keyfind({FunName, Arity1}, #function.name, Content#content.functions) of
%        false ->
%            Content;
%        Fun ->
%            NewFun = Fun#function{doc = Text},
%            Content#content{functions = lists:keyreplace({FunName, Arity1}, #function.name, Content#content.functions, NewFun)}
%    end.


