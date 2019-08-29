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


type Key =  Start | Left | Right | Jump


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
  Task.perform (\{width,height} -> ResizeWindow (width,height)) Window.size


toKey : String -> Key
toKey str = 
  case str of
    "W" ->
      Jump
    
    "A" ->
      Left
    
    "D" ->
      Right
    
    "Space" ->
      Start

keyDecoder : Decode.Decoder Key
keyDecoder =
  Decode.map toKey Decode.string
    
decodeKey : Decode.Value -> Maybe Key
decodeKey val =
    case (Decode.decodeValue keyDecoder val) of
        Ok key ->
            Just key

        Err _ ->
            Nothing
        

-- port
port getKey : (Decode.Value -> msg ) -> Sub msg

keyPressed : (Maybe Key -> msg) -> Sub msg
keyPressed toMsg = 
    getKey (\val -> toMsg (decodeKey val))