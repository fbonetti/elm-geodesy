module Geodesy
  exposing
    ( Coordinate
    , DegreesMinutesSeconds
    , Unit(..)
    , distance
    , decimalDegrees
    , degreesMinutesSeconds
    , midpoint
    , initialBearing
    , finalBearing
    , rhumbDistance
    , rhumbBearing
    , rhumbMidpoint
    )


type alias Coordinate =
  ( Float, Float )


type alias DegreesMinutesSeconds =
  ( Int, Int, Float )


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


toRadians : Float -> Float
toRadians num =
  num * pi / 180


toDegrees : Float -> Float
toDegrees num =
  num * 180 / pi


floatMod : Float -> Float -> Float
floatMod a b =
  a - (b * (toFloat << floor) (a / b))



{- Natural log -}


ln : Float -> Float
ln =
  logBase e


isFinite : Float -> Bool
isFinite num =
  (not << isNaN) num && (not << isInfinite) num



{- Converts degrees, minutes, and seconds into decimal degrees -}


decimalDegrees : DegreesMinutesSeconds -> Float
decimalDegrees ( degrees, minutes, seconds ) =
  (toFloat degrees) + ((toFloat minutes) / 60) + (seconds / 3600)



{- Converts decimal degrees into degrees, minutes, and seconds -}


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



-- FUNCTIONS


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


finalBearing : Coordinate -> Coordinate -> Float
finalBearing start destination =
  floatMod ((initialBearing destination start) + 180) 360


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
