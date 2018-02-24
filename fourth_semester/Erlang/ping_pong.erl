-module(ping_pong).
-export([
start/0,
stop/0,
play/1,
ping_loop/0,
pong_loop/0]).

start() ->
  register(ping, spawn(?MODULE, ping_loop, [])),
  register(pong, spawn(?MODULE, pong_loop, [])).

stop() ->
	ping ! stop,
	pong ! stop.

play(N) ->
  ping ! (N).

ping_loop() ->
  receive
    stop -> stopped;
    0 -> pong_loop();
    N ->
      timer:sleep(500),
      io:format("ping is sending ~n"),
      self() ! (N-1),
      ping_loop()
    after 10000 -> ok
  end.
pong_loop() ->
receive
    stop -> stopped;
    0 -> ping_loop();
    N ->
      timer:sleep(500),
      io:format("pong is sending~n"),
      self() ! (N-1),
      pong_loop()
    after 10000 -> ok
  end.
