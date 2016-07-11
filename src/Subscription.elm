module Subscription exposing (..)

import Time exposing (Time,second)

import Model exposing (..)
import Window
import Task
import AnimationFrame


type Msg
  = ResizeWindow (Int,Int)
  | Tick Time
  | NoOp

subscriptions : Model -> Sub Msg
subscriptions model =
  [ Window.resizes (\{width,height} -> ResizeWindow (width,height))
  , AnimationFrame.times Tick
  ]
  |> Sub.batch

initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
  Task.perform (\_ -> NoOp) (\{width,height} -> ResizeWindow (width,height)) Window.size
