module Tests exposing (main)

import ElmTest exposing (..)
import Geodesy exposing (..)


-- Chicago to NYC


chicagoToNycTests : List Test
chicagoToNycTests =
  [ test "Distance from Chicago to NYC" (assertEqual 1144.2995483233492 distanceChicagoToNyc)
  , test "Midpoint from Chicago to NYC" (assertEqual ( 41.49677823440974, -80.75671256248268 ) midpointFromChicagoToNyc)
  , test "Initial bearing from Chicago to NYC" (assertEqual 91.95668081107792 initialBearingFromChicagoToNyc)
  , test "Final bearing from Chicago to NYC" (assertEqual 100.97208853162614 finalBearingFromChicagoToNyc)
  , test "Rhumb line distance from Chicago to NYC" (assertEqual 1145.4782975112635 rhumbDistanceChicagoToNyc)
  , test "Rhumb midpoint from Chicago to NYC" (assertEqual ( 41.295449999999995, -80.78742529339144 ) rhumbMidpointChicagoToNyc)
  , test "Rhumb bearing from Chicago to NYC" (assertEqual 96.49514816731596 rhumbBearingChicagoToNyc)
  ]


chicago : Coordinate
chicago =
  ( 41.8781, -87.6298 )


newYork : Coordinate
newYork =
  ( 40.7128, -74.0059 )


distanceChicagoToNyc : Float
distanceChicagoToNyc =
  distance chicago newYork Kilometers


midpointFromChicagoToNyc : Coordinate
midpointFromChicagoToNyc =
  midpoint chicago newYork


initialBearingFromChicagoToNyc : Float
initialBearingFromChicagoToNyc =
  initialBearing chicago newYork


finalBearingFromChicagoToNyc : Float
finalBearingFromChicagoToNyc =
  finalBearing chicago newYork


rhumbDistanceChicagoToNyc : Float
rhumbDistanceChicagoToNyc =
  rhumbDistance chicago newYork Kilometers


rhumbBearingChicagoToNyc : Float
rhumbBearingChicagoToNyc =
  rhumbBearing chicago newYork


rhumbMidpointChicagoToNyc : Coordinate
rhumbMidpointChicagoToNyc =
  rhumbMidpoint chicago newYork



-- London to Hong Kong


londonToHongKongTests : List Test
londonToHongKongTests =
  [ test "Distance from London to Hong Kong" (assertEqual 9597.482581330738 distanceLondonToHongKong)
  , test "Midpoint from London to Hong Kong" (assertEqual ( 52.90244252102981, 73.85262787435465 ) midpointFromLondonToHongKong)
  , test "Initial bearing from London to Hong Kong" (assertEqual 57.83403659833477 initialBearingFromLondonToHongKong)
  , test "Final bearing from London to Hong Kong" (assertEqual 145.25926589781778 finalBearingFromLondonToHongKong)
  , test "Rhumb line distance from London to Hong Kong" (assertEqual 10407.501859178768 rhumbDistanceLondonToHongKong)
  , test "Rhumb midpoint from London to Hong Kong" (assertEqual ( 36.9519, 62.68985625326832 ) rhumbMidpointLondonToHongKong)
  , test "Rhumb bearing from London to Hong Kong" (assertEqual 108.12102559904315 rhumbBearingLondonToHongKong)
  ]


london : Coordinate
london =
  ( 51.5074, 0.1278 )


hongKong : Coordinate
hongKong =
  ( 22.3964, 114.1095 )


distanceLondonToHongKong : Float
distanceLondonToHongKong =
  distance london hongKong Kilometers


midpointFromLondonToHongKong : Coordinate
midpointFromLondonToHongKong =
  midpoint london hongKong


initialBearingFromLondonToHongKong : Float
initialBearingFromLondonToHongKong =
  initialBearing london hongKong


finalBearingFromLondonToHongKong : Float
finalBearingFromLondonToHongKong =
  finalBearing london hongKong


rhumbDistanceLondonToHongKong : Float
rhumbDistanceLondonToHongKong =
  rhumbDistance london hongKong Kilometers


rhumbBearingLondonToHongKong : Float
rhumbBearingLondonToHongKong =
  rhumbBearing london hongKong


rhumbMidpointLondonToHongKong : Coordinate
rhumbMidpointLondonToHongKong =
  rhumbMidpoint london hongKong



-- Suite


allTests : List Test
allTests =
  List.foldl List.append
    []
    [ chicagoToNycTests
    , londonToHongKongTests
    ]


consoleTests : Test
consoleTests =
  suite "All Tests" allTests


main : Program Never
main =
  runSuiteHtml consoleTests
