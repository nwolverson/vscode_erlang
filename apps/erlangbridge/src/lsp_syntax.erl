-module(lsp_syntax).

-export([parse_source_file/2, validate_parsed_source_file/1, compile_source_file/1, parse_config_file/2, file_syntax_tree/1, module_syntax_tree/1]).

parse_source_file(File, ContentsFile) ->
    case epp_parse_file(ContentsFile, get_include_path(File), get_define_from_rebar_config(File)) of
        {ok, FileSyntaxTree} ->
            UpdatedSyntaxTree = update_file_in_forms(File, ContentsFile, FileSyntaxTree),
            gen_lsp_doc_server:set_document_attribute(File, syntax_tree, UpdatedSyntaxTree),
            #{parse_result => true};
        _ ->
            #{parse_result => false, error_message => <<"Cannot open file">>}
    end.

validate_parsed_source_file(File) ->
    FileSyntaxTree = gen_lsp_doc_server:get_document_attribute(File, syntax_tree),
    lint(FileSyntaxTree, File).

parse_config_file(File, ContentsFile) ->
    case file:path_consult(filename:dirname(ContentsFile), ContentsFile) of
        {ok,_, _} -> #{parse_result => true};
        {error, Reason} -> #{
            parse_result => true,
            errors_warnings => [#{type => <<"error">>, 
            file => list_to_binary(File),
            info => extract_info(Reason)}] }
    end.

file_syntax_tree(File) ->
    case gen_lsp_doc_server:get_document_attribute(File, syntax_tree) of
        undefined -> 
            case epp_parse_file(File, get_include_path(File), get_define_from_rebar_config(File)) of
                {ok, FileSyntaxTree} -> FileSyntaxTree;
                _ -> throw("Cannot parse file " ++ File)
            end;
        FileSyntaxTree ->
            FileSyntaxTree
    end.

module_syntax_tree(Module) ->
    File = gen_lsp_doc_server:get_module_file(Module),
    case File of
        undefined -> undefined;
        _ -> {file_syntax_tree(File), File}
    end.

update_file_in_forms(File, File, FileSyntaxTree) ->
    FileSyntaxTree;
update_file_in_forms(File, ContentsFile, FileSyntaxTree) ->
    lists:map(fun
        ({attribute, A1, file, {FunContentsFile, A2}}) when FunContentsFile =:= ContentsFile ->
            {attribute, A1, file, {File, A2}};
        (Form) ->
            Form
    end, FileSyntaxTree).

epp_parse_file(File, IncludePath, Defines) ->
    case file:open(File, [read]) of
    {ok, FIO} -> 
        Ret = do_epp_parse_file(File, FIO, IncludePath, Defines),
        file:close(FIO), 
        Ret;
    _ -> {error, file_could_not_opened}
    end.

do_epp_parse_file(File, FIO, IncludePath, Defines) ->
    case epp:open(as_string(File), FIO, {1,1}, IncludePath, Defines) of
        {ok, Epp} -> {ok, epp:parse_file(Epp)};
        {error, _Err} -> {error, _Err} 
    end.

as_string(Text) when is_binary(Text) ->
    binary_to_list(Text);
as_string(Text) ->
    Text.

get_include_path(File) ->
    Candidates = get_standard_include_paths() ++
                get_settings_include_paths() ++
                get_file_include_paths(File) ++
                get_include_paths_from_rebar_config(File),
    Paths = lists:filter(fun filelib:is_dir/1, Candidates),
    gen_lsp_server:lsp_log("get_include_path: ~p", [Paths]),
    Paths.

get_standard_include_paths() ->
    RootDir = gen_lsp_config_server:root(),
    [
        filename:join([RootDir, "_build", "default", "lib"]),
        filename:join([RootDir, "apps"]),
        filename:join([RootDir, "lib"])
    ].

get_settings_include_paths() ->
    SettingPaths = gen_lsp_config_server:includePaths(),
    RootDir = gen_lsp_config_server:root(),
    lists:map(fun (Path) ->
        abspath(RootDir, Path)
    end, SettingPaths).

get_file_include_paths(File) ->
    Paths = [filename:dirname(File), filename:rootname(File)],
    case get_file_include_directory(File) of
        undefined ->
            Paths;
        Path ->
            [Path|Paths]
    end.

get_file_include_directory(File) ->
    case lists:reverse(filename:split(filename:dirname(File))) of
        ["src"|Rest] ->
            filename:join(lists:reverse(["include"|Rest]));
        [_, "src"|Rest] ->
            filename:join(lists:reverse(["include"|Rest]));
        _Other ->
            undefined
    end.

get_define_from_rebar_config(File) ->
    RebarConfig = find_rebar_config(filename:dirname(File)),
    case RebarConfig of
        undefined ->
            [];
        _ ->
            Consult = file:consult(RebarConfig),
            ErlOptsDefines = case Consult of
                {ok, Terms} ->
                    ErlOpts = proplists:get_value(erl_opts, Terms, []),
                    Defines = rebar_define_to_epp_define(proplists:lookup_all(d, ErlOpts)),
                    gen_lsp_server:lsp_log("get_defines: ~p", [Defines]),
                    Defines;
                _ ->
                    []
            end,
            DefaultDefines = [],
            ErlOptsDefines ++ DefaultDefines
    end.

rebar_define_to_epp_define([]) ->
    [];
rebar_define_to_epp_define([none]) ->
    [];
rebar_define_to_epp_define([H|T]) ->
    case H of 
    {d, Atom, Value} -> [{Atom, Value}] ++ rebar_define_to_epp_define(T);
    {d, Atom} -> [Atom] ++ rebar_define_to_epp_define(T);
    _ -> rebar_define_to_epp_define(T)
    end.

get_include_paths_from_rebar_config(File) ->
    RebarConfig = find_rebar_config(filename:dirname(File)),
    case RebarConfig of
        undefined ->
            [];
        _ ->
            Consult = file:consult(RebarConfig),
            ErlOptsPaths = case Consult of
                {ok, Terms} ->
                    ErlOpts = proplists:get_value(erl_opts, Terms, []),
                    IncludePaths = proplists:get_all_values(i, ErlOpts),
                    lists:map(fun (Path) ->
                        filename:absname(Path, filename:dirname(RebarConfig))
                    end, IncludePaths);
                _ ->
                    []
            end,
            DefaultPaths = [filename:dirname(RebarConfig), filename:join([filename:dirname(RebarConfig), "include"])],
            ErlOptsPaths ++ DefaultPaths
    end.

find_rebar_config(Dir) ->
    RebarConfig = filename:join(Dir, "rebar.config"),
    case filelib:is_file(RebarConfig) of
        true ->
            RebarConfig;
        _ ->
            Elements = filename:split(Dir),
            case Elements of
                [_] ->
                    undefined;
                _ ->
                    find_rebar_config(filename:join(lists:droplast(Elements)))
            end
    end.

abspath(BaseDir, Path) ->
    case filename:pathtype(Path) of
        relative ->
            filename:absname_join(BaseDir, Path);
        _ ->
            Path
    end.

%%%-----------------------------------------------------------------------------
%%% Code to try to mimic the logic in rebar3 as to where beam files are saved
%%%-----------------------------------------------------------------------------
find_rebar3_beam_dir(ErlFilePath) ->
  Clean = cleanpath(ErlFilePath),
  SrcDir = filename:dirname(Clean),
  Parts = filename:split(SrcDir),
  Location = find_rebar3_beam_dir_(lists:reverse(Parts), undefined),
  error_logger:info_msg("Guessed at ~p for ~p", [Location, ErlFilePath]),
  Location.

find_rebar3_beam_dir_(Parts = [_AppName, "lib", "default", "_build" | _Root], _MaybeAppName) ->
  parts_to_path(["ebin" | Parts]);

find_rebar3_beam_dir_(Parts = ["src" | T], MaybeAppName) ->
  error_logger:info_msg("In src dir ~p ", [Parts]),
  BasePath = parts_to_path(T),

  %% Is there an ebin directory that is a peer of src?
  PotentialPath = parts_to_path(["ebin" | T]),
  case filelib:is_dir(PotentialPath) of
    true -> PotentialPath;
    false ->
      %% Is there an app.src file?
      case filelib:wildcard(BasePath ++ "/src/*.app.src") of
        [AppFile] ->
          %% We have found the name of our OTP application
          find_rebar3_beam_dir_(T, appsrc_filename_to_app_name(AppFile));
        _ ->
          find_rebar3_beam_dir_(T, MaybeAppName)
      end
  end;


find_rebar3_beam_dir_([_ | T], undefined) ->
  find_rebar3_beam_dir_(T, undefined);

find_rebar3_beam_dir_(Parts = [_ | T], AppName) ->
  PotentialPath = parts_to_path(["ebin", AppName, "lib", "default", "_build" | Parts]),
  case filelib:is_dir(PotentialPath) of
    true -> PotentialPath;
    false -> find_rebar3_beam_dir_(T, AppName)
  end;

find_rebar3_beam_dir_([], _MaybeAppName) ->
  error_logger:info_msg("_MaybeAppName ~p~n", [_MaybeAppName]),
  "/tmp".



appsrc_filename_to_app_name(Filename) ->
  StillWithDotApp = erl_filename_to_module_name(Filename),
  filename:rootname(StillWithDotApp).


erl_filename_to_module_name(Filename) ->
  filename:rootname(filename:basename(Filename)).

parts_to_path(Parts) ->
  lists:flatten(lists:join("/", lists:reverse(Parts))).

cleanpath(Path) ->
  cleanpath(filename:split(Path), []).

cleanpath([], Acc) ->
  filename:join(lists:reverse(Acc));

cleanpath([Dot | T], Acc) when Dot == "."; Dot == <<".">> ->
  cleanpath(T, Acc);

cleanpath([DotDot | T], Acc=[_]) when DotDot == ".."; DotDot == <<"..">> ->
  cleanpath(T, Acc);

cleanpath([DotDot | T], [_|Acc]) when DotDot == ".."; DotDot == <<"..">> ->
  cleanpath(T, Acc);

cleanpath([Segment | T], Acc) ->
  cleanpath(T, [Segment | Acc]).

filename_to_outdir(File) ->
  find_rebar3_beam_dir(File).

compile_source_file(File) ->
    CompileOpts = [
        binary,
        debug_info
    ],
    case compile:file(File, CompileOpts) of 
        {ok, Mod, Bin} ->
            OutDir = filename_to_outdir(File),
            OutFile = filename:join(OutDir, atom_to_list(Mod)),
            OutBeamFile = OutFile ++ ".beam",
            case file:write_file(OutBeamFile, Bin) of
                ok -> gen_lsp_server:lsp_log("Wrote output file ~p", [OutBeamFile]), ok;
                {error, _} = Err ->
                    error_logger:error_msg("(~p) Failed to write ~p: ~p",
                                        [node(), OutFile, Err]),
                    Err
            end;
        error -> error_logger:info_msg("Not writing beam file for ~p due to error", [File]), ok;
        Z -> error_logger:error_msg("Failed to write beam file for ~p due to ~p", [File, Z]), ok
    end.

lint(FileSyntaxTree, File) ->
    LintResult = erl_lint:module(FileSyntaxTree, File,[ {strong_validation}]),
    case LintResult of
    % nothing wrong
    {ok, []} -> #{parse_result => true};
    % just warnings
    {ok, [Warnings]} ->
    #{parse_result => true,
        errors_warnings =>
            extract_error_or_warning(<<"warning">>, Warnings)};
    % errors, no warnings
    {error, [Errors], []} ->
    #{parse_result => true,
        errors_warnings =>
            extract_error_or_warning(<<"error">>, Errors)};
    % errors and warnings
    {error, [Errors], [Warnings]} ->
    #{parse_result => true,
        errors_warnings =>
            extract_error_or_warning(<<"error">>, Errors) ++
        extract_error_or_warning(<<"warning">>, Warnings)};
    {error, [], [Warnings]} ->
    #{parse_result => true,
        errors_warnings =>
            extract_error_or_warning(<<"warning">>, Warnings)};
    _Any ->
        #{parse_result => false, error_message => <<"lint error">>}
    end.

extract_error_or_warning(_Type, {_, []}) ->
    [];
extract_error_or_warning(Type, ErrorsOrWarnings) ->
    [#{type => Type,
       file =>
       erlang:list_to_binary(element(1, ErrorsOrWarnings)),
       info => extract_info(X)}
     || X <- element(2, ErrorsOrWarnings)].

extract_info({Line, Module, MessageBody}) when is_number(Line) ->
    extract_info({{Line, 1}, Module, MessageBody});
extract_info({{Line, Column}, Module, MessageBody}) ->
    % samples of X
    %{20,erl_parse,["syntax error before: ","load_xy"]}
    %{11,erl_lint,{undefined_function,{load_xy,1}}}]}
    #{
        line => Line,
        character => Column,
        message => erlang:list_to_binary(lists:flatten(apply(Module, format_error, [MessageBody]), []))
    }.
