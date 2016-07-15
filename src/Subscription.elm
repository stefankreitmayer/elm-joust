module Subscription exposing (..)

import Time exposing (Time,second)

import Model exposing (..)
import Model.Ui exposing (..)
import Window
import Task
import AnimationFrame
import Keyboard exposing (KeyCode)


type Msg
  = ResizeWindow (Int,Int)
  | Tick Time
  | KeyChange Bool KeyCode
  | StartGame
  | TimeSecond Time
  | NoOp


subscriptions : Model -> Sub Msg
subscriptions {ui} =
  let
      window = Window.resizes (\{width,height} -> ResizeWindow (width,height))
      keys = [ Keyboard.downs (KeyChange True)
             , Keyboard.ups (KeyChange False)
             ]
      animation = [ AnimationFrame.diffs Tick ]
      seconds = Time.every Time.second TimeSecond
  in
     (
     case ui.screen of
       StartScreen ->
         [ window, seconds ]

       PlayScreen ->
         [ window ] ++ keys ++ animation

       GameoverScreen ->
         [ window ] ++ keys

     ) |> Sub.batch


initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
  Task.perform (\_ -> NoOp) (\{width,height} -> ResizeWindow (width,height)) Window.size
