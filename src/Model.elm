module Model exposing (..)

import Time exposing (Time)
import Set

import Model.Ui exposing (..)
import Model.Scene exposing (..)


type alias Model =
  { ui : Ui
  , scene : Scene
  , secondsPassed : Int }


initialModel : Model
initialModel =
  { ui = initialUi
  , scene = initialScene
  , secondsPassed = 0 }


freshGame : Ui -> Model
freshGame ui =
  let
      ui' = { ui | screen = PlayScreen
                 , pressedKeys = Set.empty }
  in
      { initialModel | ui = ui' }


winScore : Int
winScore =
  5
