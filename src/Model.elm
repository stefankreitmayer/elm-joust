module Model exposing (..)

import Model.Scene exposing (..)
import Model.Ui exposing (..)
import Set
import Time exposing (Posix)


type alias Model =
    { ui : Ui
    , scene : Scene
    , secondsPassed : Int
    }


initialModel : Model
initialModel =
    { ui = initialUi
    , scene = initialScene
    , secondsPassed = 0
    }


freshGame : Ui -> Model
freshGame ui =
    let
        ui_ =
            { ui
                | screen = PlayScreen
                , pressedKeys = Set.empty
            }
    in
    { initialModel | ui = ui_ }


winScore : Int
winScore =
    5
