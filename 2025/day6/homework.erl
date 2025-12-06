-module(homework).
-export([
    calculate/1,
    read_and_process/0,
    test/0
]).

% 123 328  51 64
%  45 64  387 23
%   6 98  215 314
% *   +   *   +

% list strings representing rows of numbers
% last row is operators
% calculate of the products or sums of each column based on the operator in last row.

calculate(Rows) ->
    NumberRows = lists:sublist(Rows, length(Rows) - 1),
    OperatorRow = lists:last(Rows),
    SplitRows = [[string:trim(Num) || Num <- string:split(Row, " ", all), string:trim(Num) =/= ""] || Row <- NumberRows],

    Columns = transpose(SplitRows),
    Operators = [string:trim(Op) || Op <- string:split(OperatorRow, " ", all), string:trim(Op) =/= ""],
    Results = lists:zipwith(fun(Column, Op) ->
        Numbers = [list_to_integer(NumStr) || NumStr <- Column],
        case Op of
            "+" -> lists:sum(Numbers);
            "*" -> lists:foldl(fun(X, Acc) -> X * Acc end, 1, Numbers)
        end
    end, Columns, Operators),
    lists:sum(Results).

transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun(Row) -> hd(Row) end, M) | transpose(lists:map(fun(Row) -> tl(Row) end, M))].

test() ->
    Input = [
        "123 328 51 64",
        "45 64 387 23",
        "6 98 215 314",
        "* + * +"
    ],
    Result = calculate(Input),
    io:format("Result: ~p~n", [Result]).

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
