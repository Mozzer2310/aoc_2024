-module(day_2).

-export([
  part_1/0, 
  read_input/1, 
  total_safe_reports/1
]).

%% Completes part 1 in one call
-spec part_1() -> integer().
part_1() ->
  Reports = read_input("input/day_2.txt"),
  total_safe_reports(Reports).

%% Reads the input file into correctly formatted 'Reports'
-spec read_input(string()) -> {list(), list()}.
read_input(FileName) ->
  {ok, Data} = file:read_file(FileName),
  Lines = binary:split(Data, <<"\n">>, [global]),
  format_lines(Lines, []).

%% Determines the total number of 'safe' reports from a list of 'Reports'
total_safe_reports(Reports) ->
  lists:foldl(fun(Report, Count) ->
                 case is_safe_report(Report) of
                   true -> Count + 1;
                   false -> Count
                 end
              end,
              0,
              Reports).

%% Checks if a single report is 'safe', true for all conditions
is_safe_report(Report) ->
  is_increase_or_decrease(Report) and is_safe_difference(Report).

%% Checks if a report is increasing or decreasing
is_increase_or_decrease(Report) ->
  Ordered = lists:sort(Report),
  Report == Ordered orelse Report == lists:reverse(Ordered).

%% Checks that the difference between adjacement elements is a 'safe' distance
is_safe_difference([]) ->
  true;
is_safe_difference([_H | []]) ->
  true;
is_safe_difference([E1, E2 | Rest]) ->
  Diff = abs(E1 - E2),
  case safe_difference(Diff) of
    true ->
      is_safe_difference([E2 | Rest]);
    false ->
      false
  end.

%% Safe distance is 1-3
safe_difference(1) ->
  true;
safe_difference(2) ->
  true;
safe_difference(3) ->
  true;
safe_difference(_) ->
  false.

%% Helper function to format each line of the input
format_lines([], Reports) ->
  Reports;
format_lines([H | Rest], Reports) ->
  BinReport = binary:split(H, <<" ">>, [global]),
  Report =
    lists:reverse(
      lists:foldl(fun(E, R) -> [binary_to_integer(E) | R] end, [], BinReport)),
  format_lines(Rest, [Report | Reports]).