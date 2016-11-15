module Tests exposing (all)

import Geodesy exposing (..)
import Test exposing (..)
import Expect


-- Suite


all : Test
all =
    describe "suite"
        [ chicagoToNycTests
        , londonToHongKongTests
        ]



-- Chicago to NYC


chicagoToNycTests : Test
chicagoToNycTests =
    describe "Chicago to NYC"
        [ test "Distance from Chicago to NYC" <|
            \() -> (Expect.equal 1144.2995483233492 distanceChicagoToNyc)
        , test "Initial bearing from Chicago to NYC" <|
            \() -> (Expect.equal 91.95668081107792 initialBearingFromChicagoToNyc)
        , test "Final bearing from Chicago to NYC" <|
            \() -> (Expect.equal 100.97208853162614 finalBearingFromChicagoToNyc)
        , test "Midpoint from Chicago to NYC" <|
            \() -> (Expect.equal ( 41.49677823440974, -80.75671256248268 ) midpointFromChicagoToNyc)
        , test "Rhumb line distance from Chicago to NYC" <|
            \() -> (Expect.equal 1145.4782975112635 rhumbDistanceChicagoToNyc)
        , test "Rhumb bearing from Chicago to NYC" <|
            \() -> (Expect.equal 96.49514816731596 rhumbBearingChicagoToNyc)
        , test "Rhumb midpoint from Chicago to NYC" <|
            \() -> (Expect.equal ( 41.295449999999995, -80.78742529339144 ) rhumbMidpointChicagoToNyc)
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


initialBearingFromChicagoToNyc : Float
initialBearingFromChicagoToNyc =
    initialBearing chicago newYork


finalBearingFromChicagoToNyc : Float
finalBearingFromChicagoToNyc =
    finalBearing chicago newYork


midpointFromChicagoToNyc : Coordinate
midpointFromChicagoToNyc =
    midpoint chicago newYork


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


londonToHongKongTests : Test
londonToHongKongTests =
    describe "London to Hong Kong"
        [ test "Distance from London to Hong Kong" <|
            \() -> (Expect.equal 9597.482581330738 distanceLondonToHongKong)
        , test "Initial bearing from London to Hong Kong" <|
            \() -> (Expect.equal 57.83403659833477 initialBearingFromLondonToHongKong)
        , test "Final bearing from London to Hong Kong" <|
            \() -> (Expect.equal 145.25926589781778 finalBearingFromLondonToHongKong)
        , test "Midpoint from London to Hong Kong" <|
            \() -> (Expect.equal ( 52.90244252102981, 73.85262787435465 ) midpointFromLondonToHongKong)
        , test "Rhumb line distance from London to Hong Kong" <|
            \() -> (Expect.equal 10407.501859178768 rhumbDistanceLondonToHongKong)
        , test "Rhumb bearing from London to Hong Kong" <|
            \() -> (Expect.equal 108.12102559904315 rhumbBearingLondonToHongKong)
        , test "Rhumb midpoint from London to Hong Kong" <|
            \() -> (Expect.equal ( 36.9519, 62.68985625326832 ) rhumbMidpointLondonToHongKong)
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


initialBearingFromLondonToHongKong : Float
initialBearingFromLondonToHongKong =
    initialBearing london hongKong


finalBearingFromLondonToHongKong : Float
finalBearingFromLondonToHongKong =
    finalBearing london hongKong


midpointFromLondonToHongKong : Coordinate
midpointFromLondonToHongKong =
    midpoint london hongKong


rhumbDistanceLondonToHongKong : Float
rhumbDistanceLondonToHongKong =
    rhumbDistance london hongKong Kilometers


rhumbBearingLondonToHongKong : Float
rhumbBearingLondonToHongKong =
    rhumbBearing london hongKong


rhumbMidpointLondonToHongKong : Coordinate
rhumbMidpointLondonToHongKong =
    rhumbMidpoint london hongKong
