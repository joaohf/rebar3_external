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

lock(AppInfo, CustomState) ->
    SourceTuple = rebar_app_info:source(AppInfo),

    {external, What, _CopyTo, _Opts} = SourceTuple,

    case What of
        {http, _Url, {md5, _Checksum} = Chk} ->
            lock_http(AppInfo, Chk);
        {http, _Url, {sha256, _Checksum} = Chk} ->
            lock_http(AppInfo, Chk);
        {git, _Url} = Git ->
            lock_git(AppInfo, CustomState, Git);
        {git, _Url, _Ref} = Git ->
            lock_git(AppInfo, CustomState, Git)
    end.

lock_http(AppInfo, {md5, _Checksum}) ->
    rebar_app_info:source(AppInfo);
lock_http(AppInfo, {sha256, _Checksum}) ->
    rebar_app_info:source(AppInfo).

lock_git(AppInfo, CustomState, Git) ->
    SourceTuple = rebar_app_info:source(AppInfo),

    {external, _What, CopyTo, Opts} = SourceTuple,

    AppInfo0 = rebar_app_info:source(AppInfo, Git),
    rebar_git_resource:lock(AppInfo0, CustomState),
    {external, Git, CopyTo, Opts}.

download(TmpDir, AppInfo, RebarState, CustomState) ->
    SourceTuple = rebar_app_info:source(AppInfo),
    {external, What, _CopyTo, _Opts} = SourceTuple,

    % Download the resource defined by SourceTuple
    case What of
        {http, Url, Checksum} ->
            download_http(TmpDir, AppInfo, Url, Checksum);
        {git, _Url} = Git ->
            download_git(TmpDir, AppInfo, RebarState, CustomState, Git);
        {git, _Url, _Ref} = Git ->
            download_git(TmpDir, AppInfo, RebarState, CustomState, Git)
    end,

    ok.

download_http(TmpDir, AppInfo, Url, Checksum) ->
    SourceTuple = rebar_app_info:source(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    {external, _What, CopyTo, Opts} = SourceTuple,
    OutputCopyTo = filename:join([TmpDir, CopyTo]),

    case ensure_app(TmpDir, Name, Opts) of
        ok ->
            ok;
        {error, _Reason} = Error ->
            Error
    end,

    Basename = get_filename_from_url(Url),

    % Download the external resource
    OutputFile = filename:join(TmpDir, Basename),
    case fetch_package(Url) of
        {ok, Binary} ->
            ok = file:write_file(OutputFile, Binary);
        {error, Reason} ->
            rebar_api:abort("Unable to fetch package from ~s. Reason: ~p", [Url, Reason])
    end,

    case check_checksum(OutputFile, Checksum) of
        {ok, Chk} ->
            % This is necessary to avoid checking the file when needs_update
            % is called.
            write_checksum(OutputFile, Chk);
        {error, Expected, Current} ->
            rebar_api:abort("Invalid checksum detected for ~s. Expected: ~s Got: ~s", [
                Basename, Expected, Current
            ])
    end,

    % Unpack the resource into Output CopyTo folder.
    ok = rebar_file_utils:ensure_dir(OutputCopyTo),
    Cmd0 = lists:flatten(
        io_lib:format("tar xf ~ts -C ~ts --strip-components 1", [
            rebar_utils:escape_chars(Basename),
            rebar_utils:escape_chars(OutputCopyTo)
        ])
    ),
    rebar_utils:sh(Cmd0, [{cd, TmpDir}]),

    ok.

download_git(TmpDir, AppInfo, RebarState, CustomState, Git) ->
    SourceTuple = rebar_app_info:source(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    AppInfo0 = rebar_app_info:source(AppInfo, Git),

    {external, _What, CopyTo, Opts} = SourceTuple,

    OutputCopyTo = filename:join([TmpDir, CopyTo]),
    ok = rebar_file_utils:ensure_dir(OutputCopyTo),

    DownloadResult = rebar_git_resource:download(OutputCopyTo, AppInfo0, RebarState, CustomState),

    ok = rebar_file_utils:ensure_dir(TmpDir),

    % Some external repositories use git submodules, so let's
    % initiate git submodules here.
    {ok, _} = rebar_utils:sh("git submodule update --init --recursive", [{cd, OutputCopyTo}]),

    case ensure_app(TmpDir, Name, Opts) of
        ok ->
            DownloadResult;
        {error, _Reason} = Error ->
            Error
    end.

make_vsn(AppInfo, _CustomState) ->
    SourceTuple = rebar_app_info:source(AppInfo),

    {external, What, _CopyTo, Opts} = SourceTuple,

    case What of
        {http, _, _} ->
            Vsn = proplists:get_value(vsn, Opts, "0.1.0"),
            {plain, Vsn};
        {git, _Url} = Git ->
            AppInfo0 = rebar_app_info:source(AppInfo, Git),
            rebar_git_resource:make_vsn(AppInfo0, _CustomState);
        {git, _Url, _Ref} = Git ->
            AppInfo0 = rebar_app_info:source(AppInfo, Git),
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
        {http, Url, Checksum} ->
            needs_update_http(AppInfo, CustomState, Url, Checksum);
        {git, _Url} = Git ->
            needs_update_git(AppInfo, CustomState, Git);
        {git, _Url, _Ref} = Git ->
            needs_update_git(AppInfo, CustomState, Git)
    end.

needs_update_http(AppInfo, _CustomState, Url, {_, Checksum}) ->
    Dir = rebar_app_info:dir(AppInfo),
    ChecksumFilename = get_checksum_filename_from_url(Url),
    ChecksumPath = filename:join(Dir, ChecksumFilename),

    % Read checksum from file and check
    {ok, Bin} = file:read_file(ChecksumPath),
    ChecksumFromFile = binary_to_list(Bin),

    case Checksum =:= ChecksumFromFile of
        true ->
            false;
        false ->
            true
    end.

needs_update_git(AppInfo, CustomState, Git) ->
    SourceTuple = rebar_app_info:source(AppInfo),
    AppInfo0 = rebar_app_info:source(AppInfo, Git),

    {external, _What, CopyTo, _Opts} = SourceTuple,

    % fixup external dependency destination folder appending CopyTo to
    % dependency dir
    OutputCopyTo = filename:join(rebar_app_info:dir(AppInfo0), CopyTo),
    AppInfo1 = rebar_app_info:dir(AppInfo0, OutputCopyTo),

    rebar_git_resource:needs_update(AppInfo1, CustomState).

% Make sure there's something rebar will consider to be an app in the
% directory specified by Path.
% The return value is as specified for download/3 - Result on success or an
% 'error' tuple otherwise.
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
                        rebar_api:abort("No vsn defined for ~s", [Name]);
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

get_filename_from_url(Url) ->
    Parts = rebar_uri:parse(Url),
    Path = maps:get(path, Parts),
    filename:basename(Path).

get_checksum_filename_from_url(Url) ->
    io_lib:format("~s.checksum", [get_filename_from_url(Url)]).

check_checksum(Filename, {md5 = Type, Checksum}) ->
    FileDigest = checksum(Filename, Type, "~2.16.0B"),
    checksum_equal(Checksum, FileDigest);
check_checksum(Filename, {sha256 = Type, Checksum}) ->
    FileDigest = checksum(Filename, Type, "~64.16.0b"),
    checksum_equal(Checksum, FileDigest).

checksum(Filename, Type, FormatType) ->
    {ok, Bin} = file:read_file(Filename),
    Digest = crypto:hash(Type, Bin),
    lists:flatten([io_lib:format(FormatType, [X]) || X <- binary_to_list(Digest)]).

checksum_equal(Expected, Current) ->
    Expected0 = string:lowercase(Expected),
    Current0 = string:lowercase(Current),
    case Expected0 =:= Current0 of
        true ->
            {ok, Current0};
        false ->
            {error, Expected0, Current0}
    end.

write_checksum(Filename, Checksum) ->
    Output = io_lib:format("~s.checksum", [Filename]),
    ok = file:write_file(Output, Checksum).

fetch_package(Url) ->
    UrlBin = list_to_binary(Url),
    case rebar_httpc_adapter:request(get, UrlBin, #{}, undefined, #{}) of
        {ok, {200, _RespHeaders2, RespBody}} ->
            {ok, RespBody};
        {error, _} = Error ->
            Error
    end.
