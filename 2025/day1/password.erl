-module(password).

-export([
    get_password/1,
    process_input/0
]).

get_password(RotationList) ->
    {_, PosList} = lists:foldl(fun(Turn, {Pos, PosList}) ->
        NewPos = rotate(Turn, Pos),
        io:format("Turn: ~p, NewPos: ~p~n", [Turn, NewPos]),
        % timer:sleep(100),
        {NewPos, [NewPos | PosList]}
    end,
    {50, []},
    RotationList),
    length([Ctr || Ctr <- PosList, Ctr == 0]).

rotate([$R | NumStr], Pos) ->
    rotate_r(Pos, list_to_integer(NumStr));
rotate([$L | NumStr], Pos) ->
    rotate_l(Pos, list_to_integer(NumStr)).

rotate_r(-1, TurnCtr) ->
    rotate_r(99, TurnCtr);
rotate_r(Pos, 0) ->
    Pos;
rotate_r(Pos, TurnCtr) ->
    rotate_r(Pos-1, TurnCtr - 1).

rotate_l(100, TurnCtr) ->
    rotate_l(0, TurnCtr);
rotate_l(Pos, 0) ->
    Pos;
rotate_l(Pos, TurnCtr) ->
    rotate_l(Pos+1, TurnCtr - 1).


process_input() ->
    {ok, Bin} = file:read_file("input.txt"),
    InputLines = string:split(binary_to_list(Bin), "\n", all),
    RotationList = [string:trim(Line) || Line <- InputLines, Line =/= ""],
    Password = get_password(RotationList),
    io:format("Password: ~p~n", [Password]).
