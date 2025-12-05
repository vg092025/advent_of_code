-module(cafeteria2).

-export([
    process_input/0
]).

mrege_ranges(Ranges) ->
    SortedRanges = lists:sort(fun({S1, _E1}, {S2, _E2}) -> S1 < S2 end, Ranges),
    merge_sorted_ranges(SortedRanges, []).
merge_sorted_ranges([], Acc) ->
    lists:reverse(Acc);
merge_sorted_ranges([{S, E} | Rest], []) ->
    merge_sorted_ranges(Rest, [{S, E}]);
merge_sorted_ranges([{S, E} | Rest], [{AS, AE} | AccRest]) ->
    if
        S =< AE + 1 ->
            NewAE = max(E, AE),
            merge_sorted_ranges(Rest, [{AS, NewAE} | AccRest]);
        true ->
            merge_sorted_ranges(Rest, [{S, E}, {AS, AE} | AccRest])
    end.

% is_element_in_ranges(_, []) ->
%     false;
% is_element_in_ranges(Elem, [{S, E} | _]) when Elem >= S, Elem =< E ->
%     true;
% is_element_in_ranges(Elem, [_ | Rest]) ->
%     is_element_in_ranges(Elem, Rest).

process_input() ->
    {ok, Bin} = file:read_file("input.txt"),
    InputLines = string:split(binary_to_list(Bin), "\n", all),
    RangesLines = [string:trim(Line) || Line <- InputLines, Line =/= "", lists:member($-, Line)],
    % IngredientLines = [string:trim(Line) || Line <- InputLines, Line =/= "", not lists:member($-, Line)],

    Ranges = [parse_range(RangeStr) || RangeStr <- RangesLines],
    MergedRanges = mrege_ranges(Ranges),
    % IngredientIds = [list_to_integer(Line) || Line <- IngredientLines],

    % FreshIngredients = [Id || Id <- IngredientIds, is_element_in_ranges(Id, MergedRanges)],
    io:format("Merged Ranges: ~p~n", [MergedRanges]),
    RangeCount = count_ranges(MergedRanges),
    % io:format("Fresh Ingredients IDs: ~p~n", [FreshIngredients]),
    % io:format("Count of Fresh Ingredients: ~p~n", [length(FreshIngredients)]),
    io:format("Count of Merged Ranges: ~p~n", [RangeCount]).
parse_range(RangeStr) ->
    [StartStr, EndStr] = string:split(RangeStr, "-", all),
    {list_to_integer(string:trim(StartStr)), list_to_integer(string:trim(EndStr))}.

count_ranges(Ranges) ->
    lists:foldl(fun({S, E}, Acc) ->
        Acc + (E - S + 1)
    end, 0, Ranges).
