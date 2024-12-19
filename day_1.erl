-module(day_1).
%% Solves the Day 1: Historian Hysteria problem: https://adventofcode.com/2024/day/1
%% Summary:
%%  - Part 1: Finds the total distance between the numbers in the list after 
%%    sorting both lists and pairing them up.
-export([
    part_1/0,
    part_2/0,
    read_input/1,
    total_distance/2,
    total_similarity/2
]).

%% Completes part 1 in one call
-spec part_1() -> integer().
part_1() ->
    {List1, List2} = read_input("input/day_1.txt"),
    total_distance(List1, List2).

%% Completes part 2 in one call
part_2() ->
    {List1, List2} = read_input("input/day_1.txt"),
    total_similarity(List1, List2).

%% Reads the input file into two lists
-spec read_input(string()) -> {list(), list()}.
read_input(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Data1 = binary:split(Data, <<"\n">>, [global]),
    split_list(Data1, [], []).

%% Takes two lists and finds the total distance (difference) between them after sorting
-spec total_distance(list(), list()) -> integer().
total_distance(List1, List2) ->
    Sorted1 = lists:sort(List1),
    Sorted2 = lists:sort(List2),
    Paired = lists:zip(Sorted1, Sorted2),

    lists:foldl(fun({L, R}, Total) ->
        Total + abs(L-R)
    end, 0, Paired).

%% Takes two lists and finds the total similarity (value of element in list 1
%% multiplied by number of occurence of that element in list2)
total_similarity(List1, List2) ->
    lists:foldl(fun(Num, Total) ->
        Occurences = length(lists:filter(fun(Element) -> Element == Num end, List2)),
        Total + (Num * Occurences)
    end, 0, List1).

%% Helper function to split the triple space seperated lines of the file
split_list([], List1, List2) ->
    {List1, List2};
split_list([H | Rest], List1, List2) ->
    [BinH1, BinH2] = binary:split(H, <<"   ">>),
    H1 = binary_to_integer(BinH1),
    H2 = binary_to_integer(BinH2),
    split_list(Rest, [H1 | List1], [H2 | List2]).