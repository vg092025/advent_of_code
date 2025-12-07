-module(spliter2).

-export([
    count_paths/1,
    read_and_process/0,
    test/0
]).

test() ->
    Input = [
        ".......S.......",
        "...............",
        ".......^.......",
        "...............",
        "......^.^......",
        "...............",
        ".....^.^.^.....",
        "...............",
        "....^.^...^....",
        "...............",
        "...^.^...^.^...",
        "...............",
        "..^...^.....^..",
        "...............",
        ".^.^.^.^.^...^.",
        "..............."
    ],
    count_paths(Input).


count_paths(Input) ->
    SourceIndex = string:chr(lists:nth(1, Input), $S),
    Rows = length(Input),
    Columns = string:length(lists:nth(1, Input)),
    io:format("Rows: ~p, Columns: ~p~n", [Rows, Columns]),
     {TotalPaths, _Cache} = calculate_paths(Input, 2, SourceIndex, Rows, Columns, [{1, SourceIndex}], []),
    io:format("Total Paths: ~p~n", [TotalPaths]).

calculate_paths(_, CurrentRow, _, TotalRows, _, _PathTracker, Cache) when CurrentRow > TotalRows ->
    {1, Cache}; % reached bottom, count as 1 path
calculate_paths(Input, CurrentRow, Col, TotalRows, TotalColumns, PathTracker, Cache) ->
    CurrentRowString = lists:nth(CurrentRow, Input),
    CharAtPos = lists:nth(Col, CurrentRowString),
    CacheCount = count_from_cache(CurrentRow, Col, Cache),

    case {CacheCount, CharAtPos} of
        {Count, _} when Count > 0 ->
            {Count, Cache};
        {_, $^} ->
            {LeftPaths, Cache1} = if
                Col > 1 ->
                    calculate_paths(Input, CurrentRow + 1, Col - 1, TotalRows, TotalColumns, [{CurrentRow, Col - 1} | PathTracker], Cache);
                true ->
                    {0, Cache}
            end,
            {RightPaths, Cache2} = if
                Col < TotalColumns ->
                    calculate_paths(Input, CurrentRow + 1, Col + 1, TotalRows, TotalColumns, [{CurrentRow, Col + 1} | PathTracker], Cache1);
                true ->
                    {0, Cache1}
            end,
            Total = LeftPaths + RightPaths,
            % update cache
            NewCacheEntry = {{CurrentRow, Col}, Total},
            CacheUpdated = [NewCacheEntry | Cache2],
            {Total, CacheUpdated};
        {_, $.} ->
            % Continue straight down
            calculate_paths(Input, CurrentRow + 1, Col, TotalRows, TotalColumns, [{CurrentRow, Col} | PathTracker], Cache)
    end.
count_from_cache(_, _, []) -> 0;
count_from_cache(Row, Col, Cache) ->
    proplists:get_value({Row, Col}, Cache, -1).

read_and_process() ->
    {ok, Binary} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(Binary), "\n", all),
    TrimmedLines = [string:trim(Line) || Line <- Lines, string:trim(Line) =/= ""],
    Result = count_paths(TrimmedLines),
    io:format("Final Result: ~p~n", [Result]),
    Result.
