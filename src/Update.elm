module Update exposing (..)

import Set exposing (Set)
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
      in
          ({ model | ui = ui' }, Cmd.none)

    NoOp ->
      (model, Cmd.none)


stepPlayer : Ui -> Player -> Player
stepPlayer {pressedKeys} ({position} as player) =
  let
      direction = if keyPressed 65 pressedKeys then
              -1
           else if keyPressed 68 pressedKeys then
                   1
              else
              0
      vx = direction * 0.01
      position' = { position | x = position.x + vx }
  in
      { player | position = position' }
