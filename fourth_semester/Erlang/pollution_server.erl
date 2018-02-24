-module(pollution_server).
-export([
start/0,
stop/0,
pollution_loop/1,
call/2
]).
% [element(2, pollution:createMonitor())]
start() -> register(pollution, spawn(?MODULE, pollution_loop, [#{}])).

stop() -> pollution ! {self(), stop}.

pollution_loop(Monitor) ->
  receive
    {Pid, stop} -> Pid ! stopped;
    {Pid, Fun, Args} ->
      Margs = Args ++ [Monitor],
      try erlang:apply(pollution, Fun, Margs) of
        {ok, Value} ->
          Pid ! {ok, Value},
          pollution_loop(Monitor);
        {monitor, NewMonitor} ->
          Pid ! {monitor, NewMonitor},
          pollution_loop(NewMonitor);
        {error, Err} ->
          Pid ! {error, Err},
          pollution_loop(Monitor)
      catch
        Exception:Reason -> Pid ! {caught, Exception, Reason},
        pollution_loop(Monitor)
      end
  end.

call(Fun, Args) ->
  pollution ! {self(), Fun, Args},
  receive
    A -> A
  end.
