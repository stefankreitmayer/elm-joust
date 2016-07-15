module Update exposing (..)

import Set exposing (Set)
import Char
import Time exposing (Time)
import Keyboard exposing (KeyCode)

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Model.Geometry exposing (..)
import Subscription exposing (..)

import Debug exposing (log)
------------------------------------------------------------------------- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update action ({ui,scene} as model) =
  case action of
    ResizeWindow dimensions ->
      ({ model | ui = { ui | windowSize = dimensions } }, Cmd.none)

    Tick delta ->
      let
          player1 = scene.player1 |> steerAndGravity delta ui
          player2 = scene.player2 |> steerAndGravity delta ui
          (player1', player2') = handleCollisions player1 player2
          player1'' = player1' |> movePlayer delta
          player2'' = player2' |> movePlayer delta
          scene' = { scene | player1 = player1'', player2 = player2'' }
          hasLost player = player.position.y > 1 + playerRadius*2
          winner =
              if hasLost player1' then
                Just player2'
              else if hasLost player2' then
                Just player1'
              else
                Nothing
          screen' = case winner of
                      Just _ -> GameoverScreen
                      Nothing -> PlayScreen
          ui' = { ui | screen = screen', winner = winner }
      in
          ({ model | scene = scene', ui = ui' }, Cmd.none)

    KeyChange pressed keycode ->
      (handleKeyChange pressed keycode model, Cmd.none)

    StartGame ->
      (freshGame ui, Cmd.none)

    TimeSecond _ ->
      ({ model | secondsPassed = model.secondsPassed+1 }, Cmd.none)

    NoOp ->
      (model, Cmd.none)


steerAndGravity : Time -> Ui -> Player -> Player
steerAndGravity delta {pressedKeys} ({velocity} as player) =
  let
      directionX = if keyPressed player.leftKey pressedKeys then
                     -1
                   else if keyPressed player.rightKey pressedKeys then
                     1
                   else
                     0
      ax = directionX * 0.0000019
      vx' = velocity.x + ax*delta |> friction delta
      vy' = velocity.y |> (gravity delta)
      velocity' = { velocity | x = vx'
                             , y = vy' }
  in
     { player | velocity = velocity' }


handleCollisions : Player -> Player -> (Player,Player)
handleCollisions player1 player2 =
  if playersOverlap player1 player2 then
    bounceOffEachOther player1 player2
  else
    (player1, player2)


bounceOffEachOther : Player -> Player -> (Player,Player)
bounceOffEachOther player1 player2 =
  let
      v1 = deflect player1 player2
      v2 = deflect player2 player1
      player1' = { player1 | velocity = v1 }
      player2' = { player2 | velocity = v2 }
  in
      (player1', player2')


movePlayer : Time -> Player -> Player
movePlayer delta ({velocity,position} as player) =
  let
      vx = velocity.x
      vy = velocity.y
      airborne = position.y + vy*delta < icePosY
      stepFn = if airborne then
                 fly
               else if inBounds position.x then
                 walk
               else
                 fall
      (x', y', vx', vy') = stepFn delta position.x position.y vx vy
      position' = { position | x = x'
                             , y = y' }
      velocity' = { velocity | x = vx'
                             , y = vy' }
  in
      { player | position = position'
               , velocity = velocity' }



fly : Time -> Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
fly delta x y vx vy =
  (x+vx*delta, y+vy*delta, vx, vy)


walk : Time -> Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
walk delta x y vx vy =
  let
      x = x + vx*delta
      y = icePosY
      vy = 0
  in
      (x, y, vx, vy)


fall : Time -> Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
fall delta x y vx vy =
  let
      y = y + vy*delta
      x = x + vx*delta
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


friction : Time -> Float -> Float
friction delta vx =
  vx / (1 + 0.0018*delta)


jump : Player -> Player
jump ({position,velocity} as player) =
  let
      vy = if position.y==icePosY then -0.001 else velocity.y
      velocity' = { velocity | y = vy }
  in
      { player | velocity = velocity' }


gravity : Time -> Float -> Float
gravity delta vy =
  vy + 0.000003 * delta


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
  if x<0.5 then
     min x (icePosX-playerRadius)
  else
     max x (iceRightEdgeX+playerRadius)


handleKeyChange : Bool -> KeyCode -> Model -> Model
handleKeyChange pressed keycode ({scene,ui} as model) =
  let
      fn = if pressed then Set.insert else Set.remove
      pressedKeys' = fn keycode ui.pressedKeys
  in
      case ui.screen of
        PlayScreen ->
          let
              ui' = { ui | pressedKeys = pressedKeys' }
              justPressed keycode = freshKeyPress keycode ui.pressedKeys pressedKeys'
              maybeJump player = if justPressed player.jumpKey then
                                   jump player
                                 else
                                   player
              player1' = maybeJump scene.player1
              player2' = maybeJump scene.player2
              scene' = { scene |  player1 = player1', player2 = player2' }
          in
              { model | ui = ui', scene = scene' }

        GameoverScreen ->
          if freshKeyPress (Char.toCode ' ') ui.pressedKeys pressedKeys' then
            freshGame ui
          else
            model

        _ ->
          model


freshKeyPress : KeyCode -> Set KeyCode -> Set KeyCode -> Bool
freshKeyPress keycode previouslyPressedKeys currentlyPressedKeys =
  let
      pressed = keyPressed keycode
  in
      pressed currentlyPressedKeys && not (pressed previouslyPressedKeys)
