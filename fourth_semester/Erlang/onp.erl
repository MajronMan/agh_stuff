-module(onp).
-export([hello/0, calc_onp/1, to_onp/1]).

hello() -> io:format("helllo\n").

calc_onp(X) ->
  Toks = string:tokens(X, " "),
  [Res] = lists:foldl(fun calc_onp/2, [], Toks),
  Res.

calc_onp("+",      [N1, N2 | S]) -> [N2+N1             | S];
calc_onp("-",      [N1, N2 | S]) -> [N2-N1             | S];
calc_onp("*",      [N1, N2 | S]) -> [N2*N1             | S];
calc_onp("/",      [N1, N2 | S]) -> [N2/N1             | S];
calc_onp("^",      [N1, N2 | S]) -> [math:pow(N2, N1)  | S];
calc_onp("ln",     [N      | S]) -> [math:log(N)       | S];
calc_onp("log10",  [N      | S]) -> [math:log10(N)     | S];
calc_onp(X,                  S ) -> [read(X)           | S].

read(N) ->
  case string:to_float(N) of
    {error, no_float} -> list_to_integer(N);
    {F, _} -> F
  end.

is_operator(X) -> lists:member(X, ["+", "-", "*", "/"])

operator_weight(O) ->
  case O of
    "+" -> 1;
    "-" -> 1;
    "*" -> 2;
    "/" -> 2;
    _   -> 0;
  end.

compare_operators(O1, O2) ->
  operator_weight(O1) - operator_weight(O2).

to_onp(X) ->
  to_onp([], [], string:tokens(X, " ")).

to_onp(Out, Heap, []) -> Acc ++ Heap;
to_onp(Out, [T | H], [X | R]) when is_operator(X) ->
  if compare_operators(X, )
to_onp(Acc, Heap, [X | R]) -> to_onp(Acc ++ [read(X)], R).
