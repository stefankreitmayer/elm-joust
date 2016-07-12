module Model.Scene exposing (..)

import Model.Shared exposing (..)


type alias Scene =
  { t : Float
  , player : Player }

type alias Vector =
  { x : Float
  , y : Float }

type alias Player =
  { position : Vector
  , velocity : Vector }


initialScene : Scene
initialScene =
  { t = 0
  , player = initialPlayer }


initialPlayer : Player
initialPlayer =
  { position = { x = 0.5, y = icePosY-0.01 }
  , velocity = { x = 0, y = 0 } }


icePosY : Float
icePosY = 0.8


icePosX : Float
icePosX = 0.1


iceRightEdgeX : Float
iceRightEdgeX = icePosX + iceWidth


iceWidth : Float
iceWidth = 0.8


playerRadius : Float
playerRadius = 0.05


distance : (Float,Float) -> (Float,Float) -> Float
distance (x1,y1) (x2,y2) =
  (x2-x1)^2 + (y2-y1)^2 |> sqrt
