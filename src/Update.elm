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


update : Msg -> Model -> (Model, Cmd Msg)
update action ({ui,scene} as model) =
  case action of
    ResizeWindow dimensions ->
      ({ model | ui = { ui | windowSize = dimensions } }, Cmd.none)

    Tick delta ->
      let
          player1 = scene.player1 |> steerAndGravity delta ui
          player2 = scene.player2 |> steerAndGravity delta ui
          round = scene.round
          (player1', player2') = handleCollisions player1 player2
          player1'' = player1' |> movePlayer delta
          player2'' = player2' |> movePlayer delta
          hasAnyPlayerFallen = hasFallen player1 || hasFallen player2
          isRoundOver = hasAnyPlayerFallen && round.touchdownTime > 1400
          (player1''', player2''') = applyScores player1'' player2'' isRoundOver
          isGameOver = player1'''.score>=winScore || player2'''.score>=winScore
          (round', screen') =
            if isGameOver then
               (round, GameoverScreen)
            else if isRoundOver then
               (newRound, PlayScreen)
            else if hasAnyPlayerFallen then
              ({ round | touchdownTime = round.touchdownTime + delta }, PlayScreen)
            else
              (round, PlayScreen)
          scene' = { scene | player1 = player1'''
                           , player2 = player2'''
                           , round = round' }
          ui' = { ui | screen = screen' }
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


applyScores : Player -> Player -> Bool -> (Player,Player)
applyScores player1 player2 isRoundOver =
  if isRoundOver then
    let
        pointsForPlayer1 = if hasFallen player2 then 1 else 0
        pointsForPlayer2 = if hasFallen player1 then 1 else 0
    in
        (payAndReset player1 pointsForPlayer1
        ,payAndReset player2 pointsForPlayer2)
  else
    (player1, player2)


payAndReset : Player -> Int -> Player
payAndReset player additionalPoints =
  let
      position' = Vector player.homePosX playerHomePosY
      velocity' = Vector 0 0
  in
      { player | score = player.score + additionalPoints
               , position = position'
               , velocity = velocity' }


hasFallen : Player -> Bool
hasFallen player =
  player.position.y > 1 + playerRadius*2


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
      increment = 0.003 * (if isLeftSide then -1 else 1)
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
