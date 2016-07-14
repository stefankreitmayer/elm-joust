module Model.Scene exposing (..)

import Keyboard exposing (KeyCode)
import Char exposing (toCode)

import Model.Shared exposing (..)


type alias Scene =
  { t : Float
  , player1 : Player
  , player2 : Player }

type alias Vector =
  { x : Float
  , y : Float }

type alias Player =
  { leftKey : KeyCode
  , rightKey : KeyCode
  , jumpKey : KeyCode
  , position : Vector
  , velocity : Vector }


initialScene : Scene
initialScene =
  { t = 0
  , player1 = createPlayer 'A' 'D' 'W' 0.25
  , player2 = createPlayer 'J' 'L' 'I' 0.75 }


createPlayer : Char -> Char -> Char ->  Float -> Player
createPlayer leftKey rightKey jumpKey posX =
  { leftKey = Char.toCode leftKey
  , rightKey = Char.toCode rightKey
  , jumpKey = Char.toCode jumpKey
  , position = { x = posX, y = icePosY-0.01 }
  , velocity = { x = 0, y = 0 } }


icePosY : Float
icePosY = 0.89


icePosX : Float
icePosX = 0.1


iceRightEdgeX : Float
iceRightEdgeX = icePosX + iceWidth


iceWidth : Float
iceWidth = 0.8


playerRadius : Float
playerRadius = 0.03


distance : (Float,Float) -> (Float,Float) -> Float
distance (x1,y1) (x2,y2) =
  (x2-x1)^2 + (y2-y1)^2 |> sqrt


players : Scene -> List Player
players scene =
  [ scene.player1, scene.player2 ]
