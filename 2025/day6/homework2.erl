-module(homework2).
-export([
    calculate/1,
    read_and_process/0,
    test/0
]).

transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun(Row) -> hd(Row) end, M) | transpose(lists:map(fun(Row) -> tl(Row) end, M))].

% print for test input
test() ->
    Input = [
        "123 328  51 64 ",
        " 45 64  387 23 ",
        "  6 98  215 314",
        "*   +   *   +  "
    ],
    calculate(Input).
calculate(Input) ->
    % transpose the input except last row
    List = transpose(lists:sublist(Input, length(Input) - 1)),
    NumberLists = [string:trim(Num) || Num <- List],
    io:format("Transposed List: ~p~n", [NumberLists]),
    % ["1","24","356",[],"369","248","8",[],"32","581","175",[],"623","431","4"]

    % split numberlists into multiple rows based on empty string as separator
    SplitLists = lists:foldl(
        fun(Elem, [Current | Acc]) ->
            case Elem of
                [] -> [[] | [Current | Acc]];
                _ -> [[Elem | Current] | Acc]
            end
        end,
        [[]],
        NumberLists
    ),
    Rows = [lists:reverse(Row) || Row <- lists:reverse(SplitLists), Row =/= []],
    NumberRows = [[list_to_integer(Num) || Num <- Row] || Row <- Rows],
    io:format("Split Lists: ~p~n", [NumberRows]),

    % Split Lists: [[1,24,356],[369,248,8],[32,581,175],[623,431,4]]

    % use operator row to perform calculation
    OperatorRow = lists:last(Input),
    Operators = [string:trim(Op) || Op <- string:split(OperatorRow, " ", all), string:trim(Op) =/= ""],
    io:format("Operators: ~p~n", [Operators]),
    % Operators: ["*","+","*","+"]
    Results = lists:zipwith(fun(Column, Op) ->
        case Op of
            "+" -> lists:sum(Column);
            "*" -> lists:foldl(fun(X, Acc) -> X * Acc end, 1, Column)
        end
    end, NumberRows, Operators),
    io:format("Results: ~p~n", [Results]),
    FinalResult = lists:sum(Results),
    io:format("Final Result: ~p~n", [FinalResult]).

read_and_process() ->
    {ok, Bin} = file:read_file("input.txt"),
    InputLines = string:split(binary_to_list(Bin), "\n", all),
    CleanedLines = [string:trim(Line) || Line <- InputLines],
    FilteredLines = [Line || Line <- CleanedLines, Line =/= ""],
    case FilteredLines of
        [] ->
            io:format("Error: input.txt is empty or has no valid data~n"),
            0;
        _ ->
            Result = calculate(FilteredLines),
            io:format("Final Result: ~p~n", [Result]),
            Result

    end.
