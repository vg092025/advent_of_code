-module(password2).

-export([
    get_password/1,
    rotate_l/3,
    rotate_r/3,
    rotate/2,
    process_input/0
]).

get_password(RotationList) ->
    {_, PosList, TurnZeros } = lists:foldl(fun(Turn, {Pos, PosList, Zeros}) ->
        {NewPos, NewZeros} = rotate(Turn, Pos),
        io:format("Turn: ~p, NewPos: ~p, Zeros: ~p~n", [Turn, NewPos, NewZeros]),
        % timer:sleep(100),
        {NewPos, [NewPos | PosList], Zeros + NewZeros}
    end,
    {50, [], 0},
    RotationList),
    length([Ctr || Ctr <- PosList, Ctr == 0]) + TurnZeros.

rotate([$R | NumStr], 0) ->
    rotate_r(-1, list_to_integer(NumStr)-1, 0);
rotate([$R | NumStr], Pos) ->
    rotate_r(Pos, list_to_integer(NumStr), 0);
rotate([$L | NumStr], 0) ->
    rotate_l(1, list_to_integer(NumStr)-1, 0);
rotate([$L | NumStr], Pos) ->
    rotate_l(Pos, list_to_integer(NumStr), 0).

rotate_r(0, 0, Zerors) ->
    {0, Zerors};
rotate_r(0, TurnCtr, Zerors) ->
    io:format("rotate_r hitting zero. TurnCtr:~p, Zeros: ~p~n", [TurnCtr, Zerors]),
    rotate_r(-1, TurnCtr - 1, Zerors+1);
rotate_r(-1, TurnCtr, Zerors) ->
    rotate_r(99, TurnCtr, Zerors);
rotate_r(Pos, 0, Zerors) ->
    {Pos, Zerors};
rotate_r(Pos, TurnCtr, Zerors) ->
    rotate_r(Pos-1, TurnCtr - 1, Zerors).

rotate_l(100, 0, Zeros) ->
    {0, Zeros};
rotate_l(100, TurnCtr, Zeros) ->
    rotate_l(0, TurnCtr, Zeros + 1);
rotate_l(Pos, 0, Zeros) ->
    {Pos, Zeros};
rotate_l(Pos, TurnCtr, Zeros) ->
    rotate_l(Pos+1, TurnCtr - 1, Zeros).


process_input() ->
    {ok, Bin} = file:read_file("input.txt"),
    InputLines = string:split(binary_to_list(Bin), "\n", all),
    RotationList = [string:trim(Line) || Line <- InputLines, Line =/= ""],
    Password = get_password(RotationList),
    io:format("Password: ~p~n", [Password]).

%     Turn: "L43", NewPos: 0, Zeros: 0
% Turn: "R881", NewPos: 19, Zeros: 9
