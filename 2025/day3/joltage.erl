-module(joltage).
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
    Digits = lists:sort(fun (A, B) -> A > B end, maps:keys(DigitsMap)),

    {#{digit := D1}, #{digit := D2}} = lists:foldl(fun
        (Digit, not_found) ->
            Indices = lists:sort(fun (A, B) -> A < B end, maps:get(Digit, DigitsMap)),
            FirstIndex = lists:nth(1, Indices),
            FirstDigit = #{digit => Digit, index => FirstIndex},
            case find_2nd_digit(DigitsMap, FirstDigit) of
                not_found ->
                    not_found;
                SecondDigit ->
                    {FirstDigit, SecondDigit}
            end;
        (_, Acc) ->
            Acc
    end, not_found, Digits),
    N1 = D1 - $0,
    N2 = D2 - $0,
    N1 * 10 + N2.

find_2nd_digit(DigitsMap, #{digit := _Digit, index := Index} = _FirstDigit) ->
        Digits = lists:sort(fun (A, B) -> A > B end, maps:keys(DigitsMap)),
        lists:foldl(fun
            (D, not_found) ->
                Indices = lists:sort(fun (A, B) -> A < B end, maps:get(D, DigitsMap)),
                FilteredIndices = lists:filter(fun (I) -> I > Index end, Indices),
                case FilteredIndices of
                    [] -> not_found;
                    _ ->
                        SecondIndex = lists:nth(1, FilteredIndices),
                        #{digit => D, index => SecondIndex}
                end;
            (_, Acc) ->
                Acc
        end, not_found, Digits).

test() ->
    JoltageStr = "2712233521522212239633525221424223292522332923342263323223226223332531222232333293222213262324223122",
    find_max_joltage(JoltageStr).

process_input() ->
    {ok, Bin} = file:read_file("input.txt"),
    InputLines = string:split(binary_to_list(Bin), "\n", all),
    List = lists:map(fun
        ([]) ->
            0;
        (Line) ->
            JoltageStr = string:trim(Line),
            Res = find_max_joltage(JoltageStr),
            io:format("Max Joltage for line: ~p is ~p~n", [JoltageStr, Res]),
            Res
    end, InputLines),
    lists:sum(List).
