-module(invalid_ids).

-export([
    get_invalid_ids/1,
    get_invalid_ids/2,
    is_invalid_id/1,
    process_input/0
]).

% sample ranges in input file, all ranges in one line seperated by commas
% 69810572-69955342,3434061167-3434167492,76756725-76781020,49-147
process_input() ->
    {ok, Bin} = file:read_file("input.txt"),
    InputLines = string:split(binary_to_list(Bin), "\n", all),
    RangesLine = lists:nth(1, [string:trim(Line) || Line <- InputLines, Line =/= ""]),
    RangesStrList = string:split(RangesLine, ",", all),
    Ranges = [parse_range(RangeStr) || RangeStr <- RangesStrList],
    InvalidIds = lists:flatmap(fun({Start, End}) ->
        get_invalid_ids(Start, End)
    end, Ranges),
    io:format("Invalid IDs: ~p~n", [InvalidIds]),
    io:format("sumn of Invalid IDs: ~p~n", [lists:sum(InvalidIds)]).
parse_range(RangeStr) ->
    [StartStr, EndStr] = string:split(RangeStr, "-", all),
    {list_to_integer(string:trim(StartStr)), list_to_integer(string:trim(EndStr))}.

get_invalid_ids(Start, End) ->
    IdList = lists:seq(Start, End),
    get_invalid_ids(IdList).
get_invalid_ids(IdList) ->
    lists:filter(fun(Id) ->
        is_invalid_id(Id)
    end, IdList).
is_invalid_id(Id) ->
    Digits = length(integer_to_list(Id)),
    case Digits of
        2 -> Id rem 11 =:= 0;
        4 -> Id rem 101 =:= 0;
        6 -> Id rem 1001 =:= 0;
        8 -> Id rem 10001 =:= 0;
        10 -> Id rem 100001 =:= 0;
        12 -> Id rem 1000001 =:= 0;
        _ -> false
    end.
