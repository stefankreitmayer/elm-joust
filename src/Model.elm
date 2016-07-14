module Model exposing (..)

import Time exposing (Time)
import Set

import Model.Ui exposing (..)
import Model.Scene exposing (..)


type alias Model =
  { ui : Ui
  , scene : Scene }


initialModel : Model
initialModel =
  { ui = initialUi
  , scene = initialScene }


freshGame : Ui -> Model
freshGame ui =
  let
      ui' = { ui | screen = PlayScreen
                 , pressedKeys = Set.empty }
  in
      { initialModel | ui = ui' }
