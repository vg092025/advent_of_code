-module(printing).

-export([
    count_paper_rolls/1,
    test/0,
    process_input/0
]).

count_paper_rolls(RowList) ->
    CacheData = prepare_cache(RowList),
    RowsCtr = length(RowList),
    ColCtr = length(lists:nth(1, RowList)),
    lists:foldl(fun(R, Ctr) ->
        lists:foldl(fun(C, Acc) ->
            CellValue = get_cell_value(R, C, RowList, CacheData, RowsCtr, ColCtr),
            case is_accessible(CellValue, R, C, RowList, CacheData, RowsCtr, ColCtr) of
                true -> Acc + 1;
                false -> Acc
            end
        end, Ctr, lists:seq(1, ColCtr))

    end, 0, lists:seq(1, RowsCtr)).

is_accessible(RowValue, R, C, RowList, CacheData, RowsCtr, ColCtr) when RowValue =:= $@ ->
    RAbove = R - 1,
    RBelow = R + 1,
    CLeft = C - 1,
    CRight = C + 1,

    AboveValue = get_cell_value(RAbove, C, RowList, CacheData, RowsCtr, ColCtr),
    BelowValue = get_cell_value(RBelow, C, RowList, CacheData, RowsCtr, ColCtr),
    LeftValue = get_cell_value(R, CLeft, RowList, CacheData, RowsCtr, ColCtr),
    RightValue = get_cell_value(R, CRight, RowList, CacheData, RowsCtr, ColCtr),
    AboveRightValue = get_cell_value(RAbove, CRight, RowList, CacheData, RowsCtr, ColCtr),
    AboveLeftValue = get_cell_value(RAbove, CLeft, RowList, CacheData, RowsCtr, ColCtr),
    BelowRightValue = get_cell_value(RBelow, CRight, RowList, CacheData, RowsCtr, ColCtr),
    BelowLeftValue = get_cell_value(RBelow, CLeft, RowList, CacheData, RowsCtr, ColCtr),
    AdjacentValues = [AboveValue, BelowValue, LeftValue, RightValue,
                      AboveRightValue, AboveLeftValue, BelowRightValue, BelowLeftValue],

    % Roll value is @
    Ctr = lists:foldl(fun(V, Acc) ->
        case V of
            $@ -> Acc + 1;
            _ -> Acc
        end
    end, 0, AdjacentValues),
    io:format("Cell (~p, ~p) Adjacent @ count: ~p~n", [R, C, Ctr]),
    Ctr < 4;
is_accessible(_, _, _, _, _, _, _) ->
    false.

get_cell_value(0, _C, _RowList, _CacheData, _RowsCtr, _ColCtr) -> $.; % Out of bounds above
get_cell_value(R, _C, _RowList, _CacheData, RowsCtr, _ColCtr) when R > RowsCtr -> $.; % Out of bounds below
get_cell_value(_R, 0, _RowList, _CacheData, _RowsCtr, _ColCtr) -> $.; % Out of bounds left
get_cell_value(_R, C, _RowList, _CacheData, _RowsCtr, ColCtr) when C > ColCtr -> $.; % Out of bounds right
get_cell_value(R, C, RowList, CacheData, _RowsCtr, _ColCtr) ->
    case proplists:get_value({R, C}, CacheData) of
        undefined ->
            CellValue = fetch_cell_value(R, C, RowList),
            CellValue;
        V ->
            V
    end.

fetch_cell_value(R, C, RowList) ->
    Row = lists:nth(R, RowList),
    lists:nth(C, Row).

prepare_cache(_RowList) ->
    % todo in case of long processing, for now no caching
    [].

test() ->
    RowList = [
        ".@@@@@@@@@",
        ".@@@@@@@@@",
        ".@@@@@@@@@",
        ".@@@@@@@@@"
    ],
    PaperRolls = count_paper_rolls(RowList),
    io:format("Paper Rolls needed: ~p~n", [PaperRolls]).

process_input() ->
    {ok, Bin} = file:read_file("input.txt"),
    InputLines = string:split(binary_to_list(Bin), "\n", all),
    RowList = [string:trim(Line) || Line <- InputLines, Line =/= ""],
    PaperRolls = count_paper_rolls(RowList),
    io:format("Paper Rolls accessible: ~p~n", [PaperRolls]).
