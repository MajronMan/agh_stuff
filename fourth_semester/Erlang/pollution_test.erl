-module(pollution_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  element(2, pollution:addValue("A", {{2017, 3, 11}, {1,1,1}}, "PM10", 110,
    element(2, pollution:addValue("A", {{2017,4,22}, {19,11,13}}, "PM2,5", 1000,
      element(2, pollution:addValue("A", {{2017,4,22}, {19,11,12}}, "PM10", 100,
        element(2, pollution:addValue("A", {{2017,4,22}, {19,11,11}}, "PM10", 120,
          element(2, pollution:addStation("A", {50.23, 18.34},
            element(2, pollution:addValue("B", {{2017,4,22}, {19,11,11}}, "PM10", 410,
              element(2, pollution:addStation("B", {50.13, 19.34},
                element(2, pollution:createMonitor())
              ))
            ))
          ))
        ))
      ))
    ))
  )).

getStationMean_test() ->
  M = setup(),
  ?assert(pollution:getStationMean("A", "PM10", M) == {ok, 110.0}),
  ?assertEqual({ok, 110.0}, pollution:getStationMean({50.23, 18.34}, "PM10", M)),
  ?assertMatch({ok, 410.0}, pollution:getStationMean("B", "PM10", M)),
  ?assertEqual({error, "Cannot find station with name C"}, pollution:getStationMean("C", "asd", M)),
  ?assertEqual({error, "Cannot find station with coords {123,45}"}, pollution:getStationMean({123, 45}, "asd", M)),
  ?assertEqual({error, "No such measurements"}, pollution:getStationMean("A", "PM100", M)).

getDailyMean_test() ->
  M = setup(),
  ?assertMatch({ok, 210.0}, pollution:getDailyMean({2017,4,22}, "PM10", M)),
  ?assertMatch({ok, 110.0}, pollution:getDailyMean({2017, 3, 11}, "PM10", M)),
  ?assertMatch({ok, 1000.0}, pollution:getDailyMean({2017, 4, 22}, "PM2,5", M)),
  ?assertMatch({error, "No such measurements"}, pollution:getDailyMean({1410, 7, 15}, "PM10", M)).

getDailyOverLimit_test() ->
  M = setup(),
  ?assertEqual({ok, 2}, pollution:getDailyOverLimit({2017,4,22}, "PM10", 1, M)),
  ?assertEqual({ok, 0}, pollution:getDailyOverLimit({2017,4,22}, "PM10", 1000, M)),
  ?assertEqual({ok, 1}, pollution:getDailyOverLimit({2017,3,11}, "PM10", 100, M)).
