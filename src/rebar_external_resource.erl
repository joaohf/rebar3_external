-module(rebar_external_resource).

-export([
    init/2,
    lock/2,
    download/4,
    needs_update/2,
    make_vsn/2
]).

init(Type, _RebarState) ->
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
    {ok, Resource}.

lock(AppInfo, _CustomState) ->
    %% Extract info such as {Type, ResourcePath, ...} as declared
    %% in rebar.config
    SourceTuple = rebar_app_info:source(AppInfo),
    %% Annotate and modify the source tuple to make it absolutely
    %% and indeniably unambiguous (for example, with git this means
    %% transforming a branch name into an immutable ref)

    {external, What, CopyTo, Opts} = SourceTuple,

    case What of
        {http, _, _} = K ->
            %{external, "location", {checksum, "checksum"}};
            SourceTuple;
        Git ->
            AppInfo0 = rebar_app_info:source(AppInfo, Git),
            %AppDir = filename:join(rebar_app_info:dir(AppInfo0), "c_src"),
            %AppInfo1 = rebar_app_info:dir(AppInfo0, AppDir),

            GitSource = rebar_git_resource:lock(AppInfo0, _CustomState),

            {external, GitSource, CopyTo, Opts}
    end.

download(TmpDir, AppInfo, RebarState, CustomState) ->
    %% Extract info such as {Type, ResourcePath, ...} as declared
    %% in rebar.config
    SourceTuple = rebar_app_info:source(AppInfo),
    Name = rebar_app_info:name(AppInfo),

    %% Download the resource defined by SourceTuple, which should be
    %% an OTP application or library, into TmpDir

    {external, What, CopyTo, Opts} = SourceTuple,

    OutputCopyTo = filename:join([TmpDir, CopyTo]),

    case What of
        {http, Url, _Checksum} ->
            case ensure_app(TmpDir, Name, Opts) of
                ok ->
                    ok;
                {error, _Reason} = Error ->
                    Error
            end,

            OutputFile = filename:join(TmpDir, "binary_content"),
            Cmd = lists:flatten(
                io_lib:format(
                    "wget ~ts -O ~ts",
                    [
                        rebar_utils:escape_chars(Url),
                        rebar_utils:escape_chars(OutputFile)
                    ]
                )
            ),

            rebar_utils:sh(Cmd, [{cd, TmpDir}]),

            ok = rebar_file_utils:ensure_dir(OutputCopyTo),

            Cmd0 = lists:flatten(
                io_lib:format("tar zxf binary_content -C ~ts --strip-components 1", [
                    rebar_utils:escape_chars(OutputCopyTo)
                ])
            ),
            rebar_utils:sh(Cmd0, [{cd, TmpDir}]),

            ok;
        ElseGit ->
            AppInfo0 = rebar_app_info:source(AppInfo, ElseGit),
            R = rebar_git_resource:download(TmpDir, AppInfo0, RebarState, CustomState),

            ok = rebar_file_utils:ensure_dir(TmpDir),

            case ensure_app(TmpDir, Name, Opts) of
                ok ->
                    R;
                {error, _Reason} = Error ->
                    Error
            end
    end,

    ok.

make_vsn(AppInfo, _CustomState) ->
    %% Extract a version number from the application. This is useful
    %% when defining the version in the .app.src file as `{version, Type}',
    %% which means it should be derived from the build information. For
    %% the `git' resource, this means looking for the last tag and adding
    %% commit-specific information

    %{plain, "0.1.2"}.

    SourceTuple = rebar_app_info:source(AppInfo),

    {external, What, _CopyTo, _Opts} = SourceTuple,

    case What of
        {http, _, _} ->
            {plain, "0.1.2"};
        ElseGit ->
            AppInfo0 = rebar_app_info:source(AppInfo, ElseGit),
            %AppDir = filename:join(rebar_app_info:dir(AppInfo0), "c_src"),
            %AppInfo1 = rebar_app_info:dir(AppInfo0, AppDir),

            rebar_git_resource:make_vsn(AppInfo0, _CustomState)
    end.

needs_update(AppInfo, CustomState) ->
    %% Extract the Source tuple if needed
    SourceTuple = rebar_app_info:source(AppInfo),
    %% Base version in the current file
    %OriginalVsn = rebar_app_info:original_vsn(AppInfo),
    %% Check if the copy in the current install matches
    %% the defined value in the source tuple. On a conflict,
    %% return `true', otherwise `false'

    {external, What, _CopyTo, _Opts} = SourceTuple,

    case What of
        {http, _, _} = K ->
            false;
        ElseGit ->
            AppInfo0 = rebar_app_info:source(AppInfo, ElseGit),
            %AppDir = filename:join(rebar_app_info:dir(AppInfo0), "c_src"),
            %AppInfo1 = rebar_app_info:dir(AppInfo0, AppDir),

            rebar_git_resource:needs_update(AppInfo0, CustomState)
    end.

%
% Make sure there's something rebar will consider to be an app in the
% directory specified by Path.
% The return value is as specified for download/3 - Result on success or an
% 'error' tuple otherwise.
%
ensure_app(Path, Name, Opts) ->
    BApp = lists:flatten(
        filename:join(
            [Path, "ebin", io_lib:format("~s.app", [Name])]
        )
    ),
    SApp = lists:flatten(
        filename:join(
            [Path, "src", io_lib:format("~s.app.src", [Name])]
        )
    ),
    case filelib:is_file(BApp) orelse filelib:is_file(SApp) of
        'true' ->
            ok;
        _ ->
            Vsn =
                case proplists:get_value('vsn', Opts) of
                    'undefined' ->
                        % TODO Error
                        exit:error("no version");
                    Val ->
                        Val
                end,
            Desc = proplists:get_value('description', Opts, Name),
            Data = io_lib:format(
                "%%\n"
                "%% Generated by ~s\n"
                "%%\n"
                % this is the minimum set of elements required to make rebar
                % happy when there are no sources for it to compile
                "{application,   ~s,\n"
                "[\n"
                "    {description,   \"~s\"},\n"
                "    {vsn,           \"~s\"},\n"
                "    {modules,       []},\n"
                "    {registered,    []},\n"
                "    {applications,  [kernel, stdlib]}\n"
                "]}.\n",
                [?MODULE, Name, Desc, Vsn]
            ),
            case filelib:ensure_dir(SApp) of
                'ok' ->
                    case file:write_file(SApp, Data) of
                        'ok' ->
                            ok;
                        Err ->
                            Err
                    end;
                Err ->
                    Err
            end
    end.
