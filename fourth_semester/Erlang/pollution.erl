-module(pollution).
-include_lib("eunit/include/eunit.hrl").
-export([createMonitor/0,
addStation/3,
newCoords/2,
getStationByValue/3,
getStation/2,
getOneValue/4,
getStationMean/3,
getDailyMean/3,
addValue/5,
getMsr/3,
getMsr/4,
removeValue/4,
getDailyOverLimit/4
]).

% createMonitor/0 - tworzy i zwraca nowy monitor zanieczyszczeń;
createMonitor() -> #{}.

newCoords(Lat, Lon) -> {Lat, Lon}.

getStationByValue(Key, Val, Map) ->
  M = maps:filter(fun (_, V) -> maps:find(Key, V) == {ok, Val} end, Map),
  case maps:keys(M) of
    [H|[]] -> {ok, H};
    _ -> error
  end.

getStation(Param, Monitor) ->
  case Param of
    {Lat, Lon} -> getStationByValue(coords, {Lat, Lon}, Monitor);
    Name -> maps:find(Name, Monitor)
  end.
newStation(Name, Coords) -> #{name => Name, coords => Coords, msrs => []}.
% addStation/3 - dodaje do monitora wpis o nowej stacji
% pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
addStation(Name, Coords, Monitor) ->
  G1 = getStation(Name, Monitor),
  G2 = getStation(Coords, Monitor),
  if
    G1 /= error -> error;
    G2 /= error -> error;

    true -> maps:put(Name, newStation(Name, Coords), Monitor)
  end.


newMsr(Date, Mtype, Value) -> #{date => Date, mtype => Mtype, value => Value}.

getMsr(Date, Mtype, Value, Station) ->
  Msrs = maps:get(msrs, Station),
  case lists:filter(fun(X) -> X == newMsr(Date, Mtype, Value) end, Msrs) of
    [] -> error;
    [H | _] -> H
  end.
getMsr(Date, Mtype, Station) ->
  Msrs = maps:get(msrs, Station),
  case lists:filter(fun(X) ->
    maps:update(value, 0, X) == newMsr(Date, Mtype, 0) end, Msrs) of
    [] -> error;
    [H | _] -> H
  end.

% addValue/5 - dodaje odczyt ze stacji
% (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość),
%  zwraca zaktualizowany monitor;
addValue(Param, Date, Mtype, Value, Monitor) ->
  case getStation(Param, Monitor) of
    error -> error;
    {ok, Station} ->
      case getMsr(Date, Mtype, Station) of
        error ->
          St2 = maps:update_with(
          msrs, fun(X) ->
            lists:append(X, [newMsr(Date, Mtype, Value)])
          end, Station),
          maps:update(maps:get(name, St2), St2, Monitor);
        _ -> error
      end
  end.


% removeValue/4 - usuwa odczyt ze stacji
% (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru),
% zwraca zaktualizowany monitor;
removeValue(Param, Date, Mtype, Monitor) ->
  case getStation(Param, Monitor) of
    error -> error;
    {ok, Station} ->
    case getMsr(Date, Mtype, Station) of
      error -> error;
      Msr ->
        St2 = maps:update_with(
          msrs, fun(X) ->
            lists:delete(Msr, X) end,
          Station
        ),
        maps:update(maps:get(name, St2), St2, Monitor)
    end
  end.
% getOneValue/4 - zwraca wartość pomiaru o zadanym typie,
% z zadanej daty i stacji;
getOneValue(Param, Date, Mtype, Monitor) ->
  case getStation(Param, Monitor) of
    error -> error;
    {ok, Station} -> getMsr(Date, Mtype, Station)
  end.

getMeasuresMean([]) -> 0;
getMeasuresMean(Measures) ->
  lists:foldl(fun(A, B) -> maps:get(value, A) + B end, 0, Measures) / length(Measures).

% getStationMean/3 - zwraca średnią wartość parametru
% danego typu z zadanej stacji;
getStationMean(Param, Mtype, Monitor) ->
  case getStation(Param, Monitor) of
    error -> error;
    {ok, Station} ->
      Msrs = maps:get(msrs, Station),
      MsrsOfType = lists:filter(fun(X) ->
        maps:get(mtype, X) == Mtype end, Msrs),
        getMeasuresMean(MsrsOfType)
  end.

getDay(Measure) ->
  element(1, maps:get(date, Measure)).

% getDailyMean/3 - zwraca średnią wartość parametru danego typu,
% danego dnia na wszystkich stacjach;
getDailyMean(Mtype, Date, Monitor) ->
  Measures = lists:foldl(fun(A, B)-> maps:get(msrs, A) ++ B end, [], maps:values(Monitor)),
  DayMeasures = lists:filter(fun(X) -> (maps:get(mtype, X) == Mtype) and (getDay(X) == Date) end, Measures),
  getMeasuresMean(DayMeasures).

isMeasureAt(Date, Mtype, Measure) ->
  (maps:get(mtype, Measure) == Mtype) and (getDay(Measure) == Date).
% getDailyOverLimit/4 zwraca liczbę stacji, na których danego dnia co najmniej
% raz została przekroczona norma wartości danego parametru
getDailyOverLimit(Date, Mtype, Norm, Monitor) ->
  Stations =
    lists:filter(fun(Station) ->
      lists:any(
        fun(X) -> isMeasureAt(Date, Mtype, X) and (maps:get(value, X) > Norm) end,
      maps:get(msrs, Station))
    end, maps:values(Monitor)),
  length(Stations).

getStationMean_test_() ->
  M = addValue("A", {{2017,4,22}, {19,11,11}}, addStation("A", {1,1}, )
  ?_assert(createMonitor() == #{}),
