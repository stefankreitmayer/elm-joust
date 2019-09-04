port module Subscription exposing (..)

import Time exposing (Posix)

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (KeyCode)
import Browser.Events exposing (onAnimationFrameDelta, onResize, onKeyDown, onKeyUp)
import Task


type Msg
  = ResizeWindow (Int,Int)
  | Tick Posix
  | KeyChange Bool KeyCode
  | StartGame
  | TimeSecond Posix
  | NoOp


subscriptions : Model -> Sub Msg
subscriptions {ui} =
  let
      window = onResize (\{width,height} -> ResizeWindow (width,height))
      keys = [ onKeyDown (KeyChange True)
             , onKeyUp (KeyChange False)
             ]
      animation = [ onAnimationFrameDelta Tick ]
      seconds = Time.every 1000 TimeSecond
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
  Task.perform (\(width,height) -> ResizeWindow (width,height)) initialUi.windowSize