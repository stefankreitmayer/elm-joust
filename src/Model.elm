module Model exposing (..)

import Model.Shared exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)

import Time exposing (Time)

type alias Model =
  { ui : UIState
  , scene : Scene
  , lastFrameTime : Time }


initialModel : Model
initialModel =
  { ui = initialUi
  , scene = initialScene
  , lastFrameTime = -99999 }
