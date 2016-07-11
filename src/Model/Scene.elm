module Model.Scene exposing (..)

import Model.Shared exposing (..)


type alias Scene =
  { t : Float
  , player : Player }

type alias Point =
  { x : Float
  , y : Float }

type alias Player =
  { position : Point }


initialScene : Scene
initialScene =
  { t = 0
  , player = initialPlayer }


initialPlayer : Player
initialPlayer =
  { position = { x = 0.5, y = icePosY } }


icePosY : Float
icePosY = 0.9


icePosX : Float
icePosX = 0.1


iceWidth : Float
iceWidth = 0.8


playerRadius : Float
playerRadius = 0.05
