module Geodesy
  exposing
    ( Coordinate
    , DegreesMinutesSeconds
    , Unit(..)
    , decimalDegrees
    , degreesMinutesSeconds
    , distance
    , initialBearing
    , finalBearing
    , midpoint
    , rhumbDistance
    , rhumbBearing
    , rhumbMidpoint
    )

{-|

# Types

@docs Coordinate, DegreesMinutesSeconds, Unit

# Helpers

@docs decimalDegrees, degreesMinutesSeconds

# Great-circle path ("as the crow flies")
@docs distance, initialBearing, finalBearing, midpoint

# Rhumb line
@docs rhumbDistance, rhumbBearing, rhumbMidpoint

-}

-- TYPES


{-| Latitude & longitude pair, in decimal degrees
-}
type alias Coordinate =
  ( Float, Float )


{-| Representation of degrees in degrees, minutes, and seconds
-}
type alias DegreesMinutesSeconds =
  ( Int, Int, Float )


{-| Distance can be returned as kilometers, meters, miles, or feet
-}
type Unit
  = Kilometers
  | Meters
  | Miles
  | Feet


earthRadius : Unit -> Float
earthRadius unit =
  case unit of
    Kilometers ->
      6371.0

    Meters ->
      6371000

    Miles ->
      3958.8

    Feet ->
      5280 * 3958.8



-- HELPERS


{-| Converts degrees to radians
-}
toRadians : Float -> Float
toRadians num =
  num * pi / 180


{-| Converts radians to degrees
-}
toDegrees : Float -> Float
toDegrees num =
  num * 180 / pi


{-| Modulo function that works on float instead of ints
-}
floatMod : Float -> Float -> Float
floatMod a b =
  a - (b * (toFloat << floor) (a / b))


{-| Natural log
-}
ln : Float -> Float
ln =
  logBase e


{-| Returns false if NaN, +infinity, or -infinity
-}
isFinite : Float -> Bool
isFinite num =
  (not << isNaN) num && (not << isInfinite) num



-- FUNCTIONS


{-| Converts degrees, minutes, and seconds into decimal degrees
-}
decimalDegrees : DegreesMinutesSeconds -> Float
decimalDegrees ( degrees, minutes, seconds ) =
  (toFloat degrees) + ((toFloat minutes) / 60) + (seconds / 3600)


{-| Converts decimal degrees into degrees, minutes, and seconds
-}
degreesMinutesSeconds : Float -> DegreesMinutesSeconds
degreesMinutesSeconds decimal =
  let
    degrees =
      floor decimal

    minutes =
      (decimal - (toFloat degrees)) * 60

    minutes' =
      floor minutes

    seconds =
      (minutes - (toFloat minutes')) * 60
  in
    ( degrees, minutes', seconds )


{-| Great-circle distance between two points on a sphere
-}
distance : Coordinate -> Coordinate -> Unit -> Float
distance ( lat1, lon1 ) ( lat2, lon2 ) unit =
  let
    lat1' =
      toRadians lat1

    lat2' =
      toRadians lat2

    lon1' =
      toRadians lon1

    lon2' =
      toRadians lon2

    radius =
      earthRadius unit

    deltaLat =
      lat2' - lat1'

    deltaLon =
      lon2' - lon1'

    a =
      sin (deltaLat / 2)
        * sin (deltaLat / 2)
        + cos lat1'
        * cos lat2'
        * sin (deltaLon / 2)
        * sin (deltaLon / 2)

    c =
      2 * (atan2 (sqrt a) (sqrt (1 - a)))
  in
    radius * c


{-| Gives the initial compass bearing of a great-circle path
-}
initialBearing : Coordinate -> Coordinate -> Float
initialBearing ( lat1, lon1 ) ( lat2, lon2 ) =
  let
    lat1' =
      toRadians lat1

    lat2' =
      toRadians lat2

    deltaLon =
      toRadians (lon2 - lon1)

    y =
      sin (deltaLon) * cos lat2'

    x =
      cos lat1' * sin lat2' - sin lat1' * cos lat2' * cos deltaLon
  in
    toDegrees (atan2 y x)


{-| Gives the final compass bearing of a great-circle path
-}
finalBearing : Coordinate -> Coordinate -> Float
finalBearing start destination =
  floatMod ((initialBearing destination start) + 180) 360


{-| Half-way point along a great circle path between the two points
-}
midpoint : Coordinate -> Coordinate -> Coordinate
midpoint ( lat1, lon1 ) ( lat2, lon2 ) =
  let
    lat1' =
      toRadians lat1

    lon1' =
      toRadians lon1

    lat2' =
      toRadians lat2

    deltaLon =
      toRadians (lon2 - lon1)

    bx =
      cos lat2' * cos deltaLon

    by =
      cos lat2' * sin deltaLon

    x =
      sqrt (((cos lat1') + bx) * ((cos lat1') + bx) + by * by)

    y =
      sin lat1' + sin lat2'

    lat3 =
      atan2 y x

    lon3 =
      lon1' + (atan2 by (cos lat1' + bx))

    lon3' =
      floatMod ((toDegrees lon3) + 540) 360 - 180
  in
    ( toDegrees lat3, lon3' )


{-| Rhumb line distance between two points
-}
rhumbDistance : Coordinate -> Coordinate -> Unit -> Float
rhumbDistance ( lat1, lon1 ) ( lat2, lon2 ) unit =
  let
    radius =
      earthRadius unit

    lat1' =
      toRadians lat1

    lat2' =
      toRadians lat2

    deltaLat =
      lat2' - lat1'

    deltaLon =
      (toRadians << abs) (lon2 - lon1)

    deltaLon' =
      if abs deltaLon > pi then
        if deltaLon > 0 then
          2 * pi - deltaLon
        else
          2 * pi + deltaLon
      else
        deltaLon

    projectedLatDelta =
      ln ((tan (lat2' / 2 + pi / 4)) / tan (lat1' / 2 + pi / 4))

    q =
      if abs projectedLatDelta > 1.0e-11 then
        deltaLat / projectedLatDelta
      else
        cos lat1'

    a =
      sqrt (deltaLat * deltaLat + q * q * deltaLon' * deltaLon')
  in
    a * radius


{-| Constant compass bearing needed to traverse a rhumb line
-}
rhumbBearing : Coordinate -> Coordinate -> Float
rhumbBearing ( lat1, lon1 ) ( lat2, lon2 ) =
  let
    lat1' =
      toRadians lat1

    lat2' =
      toRadians lat2

    deltaLon =
      toRadians (lon2 - lon1)

    deltaLon' =
      if abs deltaLon > pi then
        if deltaLon > 0 then
          negate (2 * pi - deltaLon)
        else
          2 * pi + deltaLon
      else
        deltaLon

    deltaPsi =
      ln ((tan (lat2' / 2 + pi / 4)) / (tan (lat1' / 2 + pi / 4)))

    theta =
      atan2 deltaLon' deltaPsi
  in
    ((toDegrees theta) + 360) `floatMod` 360


{-| Half-way point along a rhumb line
-}
rhumbMidpoint : Coordinate -> Coordinate -> Coordinate
rhumbMidpoint ( lat1, lon1 ) ( lat2, lon2 ) =
  let
    lat1' =
      toRadians lat1

    lat2' =
      toRadians lat2

    lon1' =
      toRadians lon1

    lon2' =
      toRadians lon2

    lon1'' =
      if abs (lon2' - lon1') > pi then
        lon1' + 2 * pi
      else
        lon1'

    lat3 =
      (lat1' + lat2') / 2

    f1 =
      tan (pi / 4 + lat1' / 2)

    f2 =
      tan (pi / 4 + lat2' / 2)

    f3 =
      tan (pi / 4 + lat3 / 2)

    lon3 =
      ((lon2' - lon1'') * ln f3 + lon1'' * ln f2 - lon2' * ln f1) / (ln (f2 / f1))

    lon3' =
      if (not << isFinite) lon3 then
        (lon1'' + lon2') / 2
      else
        lon3

    lat3Degrees =
      toDegrees lat3

    lon3Degrees =
      (floatMod ((toDegrees lon3') + 540) 360) - 180
  in
    ( lat3Degrees, lon3Degrees )
