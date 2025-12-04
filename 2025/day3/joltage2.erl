-module(joltage2).
-export([
    process_input/0,
    find_max_joltage/1,
    test/0
]).

find_max_joltage(JoltageList) ->
    {DigitsMap, _} = lists:foldl(fun(Joltage, {Acc, Index}) ->
        case maps:get(Joltage, Acc, []) of
            [] ->{ maps:put(Joltage, [Index], Acc), Index + 1};
            Indices -> {maps:put(Joltage, [Index | Indices], Acc), Index + 1}
        end
    end, {#{}, 1}, JoltageList),
    find_n_indices(DigitsMap, [], 12, {0, -1}, length(JoltageList)).

 find_n_indices(_DigitsMap, NumberAcc, 0, _, _) ->
    NumberAcc;

 find_n_indices(DigitsMap, NumberAcc, N, {_LDigit, LIndex}, Length) ->
    IndexAcc = [ I || {_D, I} <- NumberAcc],
    Digits = lists:sort(fun (A, B) -> A > B end, maps:keys(DigitsMap)),
    NewIndices = lists:foldl(fun
        (D, not_found) ->
            Indices = lists:sort(fun (A, B) -> A < B end, maps:get(D, DigitsMap)),

            FilteredIndices = lists:filter(
                fun (I) ->
                    R1 = lists:all(fun (PrevI) -> I > PrevI end, IndexAcc),
                    R2 = (I > LIndex),
                    R3 = I =< (Length - N + 1),
                    R1 andalso R2 andalso R3
                end,
            Indices),
            case FilteredIndices of
                [] ->
                    not_found;
                _ ->
                    NextIndex = lists:nth(1, FilteredIndices),
                    [{D, NextIndex} | NumberAcc]
            end;
        (_, Acc) ->
            Acc
    end, not_found, Digits),

    case NewIndices of
        not_found ->
            [LastDigit | Rest] = NumberAcc,
            find_n_indices(DigitsMap, Rest, N+1, LastDigit, Length);
        _ ->
            find_n_indices(DigitsMap, NewIndices, N-1, {0, -1}, Length)
    end.

test() ->
    JoltageStr = "2712233521522212239633525221424223292522332923342263323223226223332531222232333293222213262324223122",
    find_max_joltage(JoltageStr).

process_input() ->
    {ok, Bin} = file:read_file("input.txt"),
    InputLines = string:split(binary_to_list(Bin), "\n", all),
    List = lists:map(fun
        ([]) ->
            "0";
        (Line) ->
            JoltageStr = string:trim(Line),
            Res = find_max_joltage(JoltageStr),
            SortedRes = lists:sort(fun ({_D1, I1}, {_D2, I2}) -> I1 < I2 end, Res),
            [ D || {D, _I} <- SortedRes ]
    end, InputLines),
    lists:sum([list_to_integer(L) || L <- List]).
