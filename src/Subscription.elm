module Subscription exposing (..)

import Time exposing (Time,second)

import Model exposing (..)
import Window
import Task
import AnimationFrame
import Keyboard exposing (KeyCode)


type Msg
  = ResizeWindow (Int,Int)
  | Tick Time
  | KeyChange Bool KeyCode
  | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
  [ Window.resizes (\{width,height} -> ResizeWindow (width,height))
  , Keyboard.downs (KeyChange True)
  , Keyboard.ups (KeyChange False)
  , AnimationFrame.times Tick
  ]
  |> Sub.batch


initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
  Task.perform (\_ -> NoOp) (\{width,height} -> ResizeWindow (width,height)) Window.size
