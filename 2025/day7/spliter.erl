-module(spliter).

-export([
    count_splits/1,
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
    count_splits(Input).


count_splits(Input) ->
    % get source in 1st row which is represented by 'S'
    SourceIndex = string:chr(lists:nth(1, Input), $S),
    io:format("Source Index: ~p~n", [SourceIndex]),

    % now loop next row, as S is source of beam, so next '.' will be replated by beam '|'
    Rows = length(Input),
    Columns = string:length(lists:nth(1, Input)),
    io:format("Rows: ~p, Columns: ~p~n", [Rows, Columns]),
    BeamRowPositions = #{1 => [SourceIndex]},
    beam_travel(Input, 2, BeamRowPositions, Rows, Columns, 0).

beam_travel(_, CurrentRow, BeamRowPositions, TotalRows, _, SplitCount) when CurrentRow > TotalRows ->
    {SplitCount, BeamRowPositions};
beam_travel(Input, CurrentRow, BeamRowPositions, TotalRows, TotalColumns, SplitCount) ->
    CurrentRowString = lists:nth(CurrentRow, Input),
    io:format("Processing Row ~p: ~s~n", [CurrentRow, CurrentRowString]),
    % get current beam positions from previous row
    PreviousBeamPositions = maps:get(CurrentRow - 1, BeamRowPositions, []),
    io:format("Previous Beam Positions: ~p~n", [PreviousBeamPositions]),
    % find new beam positions in current row
    {NewBeamPositions, NewSplits} = find_beam_positions(CurrentRowString, PreviousBeamPositions, TotalColumns),
    io:format("New Beam Positions: ~p, New Splits: ~p~n", [NewBeamPositions, NewSplits]),
    % update BeamRowPositions map
    UpdatedBeamRowPositions = maps:put(CurrentRow, NewBeamPositions, BeamRowPositions),
    % continue to next row
    beam_travel(Input, CurrentRow + 1, UpdatedBeamRowPositions, TotalRows, TotalColumns, SplitCount + NewSplits).

find_beam_positions(RowString, PreviousBeamPositions, TotalColumns) ->
    lists:foldl(
        fun(Pos, {BeamPositionsAcc, SplitCountAcc}) ->
            CharAtPos = lists:nth(Pos, RowString),
            case CharAtPos of
                $^ ->
                    NewPositions = [],
                    NewPositions1 = if Pos > 1 -> [Pos - 1 | NewPositions]; true -> NewPositions end,
                    NewPositions2 = if Pos < TotalColumns -> [Pos + 1 | NewPositions1]; true -> NewPositions1 end,
                    { lists:usort(BeamPositionsAcc ++ NewPositions2), SplitCountAcc + 1};
                $. ->
                    % continue beam straight down
                    {BeamPositionsAcc ++ [Pos], SplitCountAcc};
                _ ->
                    % no beam
                    {BeamPositionsAcc, SplitCountAcc}
            end
        end,
        {[], 0},
        PreviousBeamPositions
    ).

read_and_process() ->
    % read input from file
    {ok, Binary} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(Binary), "\n", all),
    TrimmedLines = [string:trim(Line) || Line <- Lines, string:trim(Line) =/= ""],
    {Count, _} = count_splits(TrimmedLines),
    io:format("Total Splits: ~p~n", [Count]).
