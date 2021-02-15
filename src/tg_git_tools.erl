-module(tg_git_tools).

-export([
    find_git/1,
    get_latest_sha/2
]).

-include_lib("kernel/include/file.hrl").

find_git("") ->
    error;
find_git(Dir) ->
    io:format("Checking ~s~n", [Dir]),
    case file:read_file_info(Dir ++ "/.git") of
        {ok, #file_info{type = directory}} ->
            {ok, Dir};
        _ ->
            Parent = tg_file_tools:parent_dir(Dir),
            find_git(Parent)
    end.

get_latest_sha(BasePath, File) ->
    Git = find_git(),
    {Out, 0} = do_cmd(
        open_port(
            {spawn_executable, Git},
            [
                use_stdio,
                exit_status,
                binary,
                hide,
                {cd, BasePath},
                {args, ["log", "-n1", "--format=format:%H", File]}
            ]
        )
    ),
    unicode:characters_to_list(Out).

find_git() ->
    os:find_executable("git").

do_cmd(Port) -> do_cmd(Port, <<>>).

do_cmd(Port, Output) ->
    receive
        {Port, {data, Data}} ->
            do_cmd(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, Status}} ->
            {Output, Status}
    end.
