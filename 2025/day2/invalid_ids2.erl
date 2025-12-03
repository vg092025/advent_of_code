-module(invalid_ids2).

-export([
    get_invalid_ids/1,
    get_invalid_ids/2,
    is_invalid_id/1,
    process_input/0,
    test/0
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
        3 -> Id rem 111 =:= 0;
        4 -> Id rem 1111 =:= 0 orelse Id rem 101 =:= 0;
        5 -> Id rem 11111 =:= 0;
        6 -> Id rem 111_111 =:= 0 orelse Id rem 10101 =:= 0 orelse Id rem 1001 =:= 0;
        7 -> Id rem 1_111_111 =:= 0;
        8 ->
            Id rem 11_11_11_11 =:= 0 orelse Id rem 10_10_10_1 =:= 0 orelse
                Id rem 10_001 =:= 0;
        9 -> Id rem 111_111_111 =:= 0 orelse Id rem 100_100_1 =:= 0;
        10 ->
            Id rem 1_111_111_111 =:= 0 orelse Id rem 10_10_10_10_1 =:= 0 orelse
                Id rem 100_001 =:= 0;
        11 -> Id rem 111_111_111_11 =:= 0;
        12 ->
            Id rem 1_111_111_111_111 =:= 0 orelse Id rem 10_10_10_10_10_1 =:= 0 orelse
                Id rem 100_100_1001 =:= 0 orelse Id rem 1000_1000_1 =:= 0 orelse
                Id rem 1000001 =:= 0;
        _ -> false
    end.

test() ->
    ValidIds = [
        11, 22,
        555,
        1010, 2020, 1111,
        11111,
        222222, 121212, 123123,
        3333333,
        88888888, 12121212, 12341234,
        999999999, 123123123,
        5555555555, 101010101010, 1234512345,
        22222222222,
        666666666666, 121212121212, 123123123123, 123412341234, 123456123456
    ],
    lists:foreach(fun(Id) ->
        case is_invalid_id(Id) of
            true -> ok; %io:format("Error: Valid ID ~p marked as invalid~n", [Id]);
            false -> io:format("Valid ID ~p correctly marked as valid~n", [Id])
        end
    end, ValidIds).
