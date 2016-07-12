module Update exposing (..)

import Set exposing (Set)
import Char
import Keyboard exposing (KeyCode)

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Subscription exposing (..)

import Debug exposing (log)
------------------------------------------------------------------------- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update action ({ui,scene} as model) =
  case action of
    ResizeWindow dimensions ->
      ({ model | ui = { ui | windowSize = dimensions } }, Cmd.none)

    Tick time ->
      let
          player' = stepPlayer ui scene.player
          scene' = { scene | t = time, player = player' }
      in
         if time > model.lastRender+40 then
           ({ model | scene = scene', lastRender = time }, Cmd.none)
         else
           (model, Cmd.none)

    KeyChange pressed keycode ->
      let
          pressedKeys' =  (if pressed then Set.insert else Set.remove) keycode ui.pressedKeys
          ui' = { ui | pressedKeys = pressedKeys' }
          player' = handleJump ui.pressedKeys pressedKeys' scene.player
          scene' = { scene |  player = player' }
      in
          ({ model | ui = ui', scene = scene' }, Cmd.none)

    NoOp ->
      (model, Cmd.none)


stepPlayer : Ui -> Player -> Player
stepPlayer {pressedKeys} ({position,velocity} as player) =
  let
      directionX = if keyPressed (Char.toCode 'A') pressedKeys then
                     -1
                   else if keyPressed (Char.toCode 'D') pressedKeys then
                     1
                   else
                     0
      ax = directionX * 0.007
      vx = velocity.x + ax |> friction |> speedLimit
      vy = velocity.y |> gravity
      x = position.x + vx
      y = position.y + vy
      (y', vy') = ground y vy
      position' = { position | x = x
                             , y = y' }
      velocity' = { velocity | x = vx
                             , y = vy' }
  in
      { player
      | position = position'
      , velocity = velocity' }


speedLimit : Float -> Float
speedLimit vx =
  let
      maxSpeed = 0.03
  in
      vx
      |> max -maxSpeed
      |> min maxSpeed


friction : Float -> Float
friction vx =
  vx * 0.88


handleJump : Set KeyCode -> Set KeyCode -> Player -> Player
handleJump keyPressedBefore keyPressedNow ({position,velocity} as player) =
  let
      upKey = Char.toCode 'W'
      wasPressed = keyPressed upKey keyPressedBefore
      isPressed = keyPressed upKey keyPressedNow
      vy = if position.y==icePosY && isPressed && (not wasPressed) then -0.07 else velocity.y
      velocity' = { velocity | y = vy }
  in
      { player | velocity = velocity' }


gravity : Float -> Float
gravity vy =
  vy + 0.008


ground : Float -> Float -> (Float,Float)
ground y vy =
  let
      vy = if y>=icePosY && vy>=0 then 0 else vy
      y = y |> min icePosY
  in
      (y, vy)
