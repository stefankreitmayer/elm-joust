module Model.Geometry exposing (..)


type alias Vector =
  { x : Float
  , y : Float }


distance : (Float,Float) -> (Float,Float) -> Float
distance (x1,y1) (x2,y2) =
  (x2-x1)^2 + (y2-y1)^2 |> sqrt


magnitude : Vector -> Float
magnitude {x,y} =
  x^2 + y^2 |> sqrt


angleBetweenPoints : Vector -> Vector -> Float
angleBetweenPoints a b =
  atan2 (b.y-a.y) (b.x-a.x)
