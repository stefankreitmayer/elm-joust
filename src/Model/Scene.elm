module Model.Scene exposing (..)

import Keyboard exposing (KeyCode)
import Char exposing (toCode)
import Time exposing (Time)

import Model.Geometry exposing (..)


type alias Scene =
  { t : Time
  , player1 : Player
  , player2 : Player
  , round : Round }

type alias Player =
  { score : Int
  , homePosX : Float
  , leftKey : KeyCode
  , rightKey : KeyCode
  , jumpKey : KeyCode
  , position : Vector
  , velocity : Vector }

type alias Round =
  { touchdownTime : Time }

initialScene : Scene
initialScene =
  { t = 0
  , player1 = createPlayer 'A' 'D' 'W' 0.25
  , player2 = createPlayer 'J' 'L' 'I' 0.75
  , round = newRound }


createPlayer : Char -> Char -> Char ->  Float -> Player
createPlayer leftKey rightKey jumpKey posX =
  { score = 0
  , homePosX = posX
  , leftKey = Char.toCode leftKey
  , rightKey = Char.toCode rightKey
  , jumpKey = Char.toCode jumpKey
  , position = { x = posX, y = playerHomePosY }
  , velocity = { x = 0, y = 0 } }


newRound : Round
newRound =
  { touchdownTime = 0 }


playerHomePosY : Float
playerHomePosY =
  icePosY-0.2


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


players : Scene -> List Player
players scene =
  [ scene.player1, scene.player2 ]


playersOverlap : Player -> Player -> Bool
playersOverlap p1 p2 =
  let
      d = distance (p1.position.x,p1.position.y) (p2.position.x,p2.position.y)
  in
      d < playerRadius*2


deflect : Player -> Player -> Vector
deflect player otherPlayer =
  let
      power = magnitude otherPlayer.velocity
      angle = angleBetweenPoints player.position otherPlayer.position |> (+) pi
      vx = cos angle |> (*) power
      vy = sin angle |> (*) power
  in
      { x = vx, y = vy }
