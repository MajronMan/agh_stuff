-module(mulserwer).
-export([start/0, stop/0, mul/1]).
-export([loop/0]).
start() -> register (m3server, spawn (?MODULE, loop, [])).
loop() ->
  receive
    stop -> ok;
    {Pid, N} ->
      Result = N * 3,
      io:format("Server got ~w, returning ~w ~n", [N, Result]),
      Pid ! Result,
      mulserwer:loop()
  end.
stop() -> m3server ! stop.
mul(N) ->
  m3server ! {self(), N},
  receive
    M -> M
  end,
  io:format("Result: ~p~n", [M]).
