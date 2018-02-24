-module(lab2).
-export([lessThan/2, grtEqThan/2, qs/1, randomElems/3, compareSpeeds/3, map/2,
filter/2, sumDigits/1
]).

lessThan(List, Arg) -> [X || X<-List, X<Arg].
grtEqThan(List, Arg) -> [X || X<-List, X>=Arg].

qs([]) -> [];
qs([Pivot|[]]) -> [Pivot];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ) .

randomElemsInner(Acc, 0, _, _) -> Acc;
randomElemsInner(Acc, N, Min, Max) -> randomElemsInner(
  Acc++[rand:uniform(Max-Min)+Min], N-1, Min, Max
  ).
randomElems(N, Min, Max) -> randomElemsInner([], N, Min, Max).
compareSpeeds(List, Fun1, Fun2) ->
  {T1, _} = timer:tc(Fun1, [List]),
  {T2, _ } = timer:tc(Fun2, [List]),
  %io:format("t1: ~w, t2: ~w ~n", T1, T2).
  {T1, T2}.

map([], _) -> [];
map([H|T], Fun) -> [Fun(H)] ++ map(T, Fun).

filter(List, Pred) -> [X || X <- List, Pred(X)].

sumDigits(X) ->
  L = integer_to_list(X),
  LL = lists:map(fun(A) -> A-48 end, L),
  lists:foldl(fun (A, Sum) -> A+Sum end, 0, LL).
