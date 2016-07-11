module Util exposing (..)

pi = 3.1415926536


interpolate : Float -> Float -> Float -> Float
interpolate a b p =
  a + (b-a) * p
