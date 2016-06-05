module Main exposing (main)

import ElmTest exposing (..)
import Geodesy exposing (..)

chicagoToNycTests : List Test
chicagoToNycTests =
  [ test "Distance from Chicago to NYC" (assertEqual 1144 (round distanceChicagoToNyc))
  , test "Midpoint from Chicago to NYC" (assertEqual (41.49677823440974,-80.75671256248268) (midpoint chicago newYork))
  , test "Initial bearing from Chicago to NYC" (assertEqual 92 (round (initialBearing chicago newYork)))
  , test "Final bearing from Chicago to NYC" (assertEqual 101 (round (finalBearing chicago newYork)))
  , test "Rhumb line distance from Chicago to NYC" (assertEqual 1145 (round rhumbDistanceChicagoToNyc))
  ]

distanceChicagoToNyc : Float
distanceChicagoToNyc =
  distance chicago newYork Kilometers

rhumbDistanceChicagoToNyc : Float
rhumbDistanceChicagoToNyc =
  rhumbDistance chicago newYork Kilometers

chicago : Coordinate
chicago =
  (41.8781, -87.6298)

newYork : Coordinate
newYork =
  (40.7128, -74.0059)

allTests : List Test
allTests =
  List.foldl
    List.append
    []
    [ chicagoToNycTests ]

consoleTests : Test
consoleTests =
  suite "All Tests" allTests

main : Program Never
main =
  runSuiteHtml consoleTests
