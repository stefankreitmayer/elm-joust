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
      airborne = position.y + vy < icePosY
      stepFn = if airborne then
                 fly
               else if inBounds position.x then
                 walk
               else
                 fall
      (x', y', vx', vy') = stepFn position.x position.y vx vy
      position' = { position | x = x'
                             , y = y' }
      velocity' = { velocity | x = vx'
                             , y = vy' }
  in
      { player
      | position = position'
      , velocity = velocity' }


fly : Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
fly x y vx vy =
  (x+vx, y+vy, vx, vy)


walk : Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
walk x y vx vy =
  let
      x = x + vx
      y = icePosY
      vy = 0
  in
      (x, y, vx, vy)


fall : Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
fall x y vx vy =
  let
      y = y + vy
      x = x + vx
      isLeftSide = x<0.5
      x' = if y<icePosY+playerRadius then
             rollOffEdge x y isLeftSide
           else
             keepOutOfBounds x
  in
      (x', y, vx, vy)


inBounds : Float -> Bool
inBounds x =
  x>=icePosX && x<=iceRightEdgeX


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


rollOffEdge : Float -> Float -> Bool -> Float
rollOffEdge x y isLeftSide =
  let
      edgePosX = if isLeftSide then icePosX else iceRightEdgeX
      increment = 0.001 * (if isLeftSide then -1 else 1)
  in
      if distance (x,y-playerRadius) (edgePosX,icePosY) > playerRadius then
        x
      else
        rollOffEdge (x + increment) y isLeftSide


keepOutOfBounds : Float -> Float
keepOutOfBounds x =
  if x<0.5 then min x (icePosX-playerRadius) else max x (iceRightEdgeX+playerRadius)
