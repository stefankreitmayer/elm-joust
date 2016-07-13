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
  | ClickToPlay
  | NoOp


subscriptions : Model -> Sub Msg
subscriptions {ui} =
  let
      window = Window.resizes (\{width,height} -> ResizeWindow (width,height))
      play = [ Keyboard.downs (KeyChange True)
             , Keyboard.ups (KeyChange False)
             , AnimationFrame.times Tick
             ]
  in
     (
     case ui.screen of
       StartScreen ->
         [ window ]

       PlayScreen ->
         [ window ] ++ play

       GameoverScreen ->
         [ window ]

     ) |> Sub.batch


initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
  Task.perform (\_ -> NoOp) (\{width,height} -> ResizeWindow (width,height)) Window.size
