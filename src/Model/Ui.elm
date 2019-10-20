module Model.Ui exposing (..)

import Model.Scene exposing (..)
import Set exposing (Set)


type alias Ui =
    { windowSize : ( Int, Int )
    , pressedKeys : Set KeyCode
    , screen : Screen
    }


type Screen
    = StartScreen
    | PlayScreen
    | GameoverScreen


initialUi : Ui
initialUi =
    { windowSize = ( 500, 500 )
    , pressedKeys = Set.empty
    , screen = StartScreen
    }

setWindowSize : (Int, Int) -> Ui -> Ui
setWindowSize size ui =
  {ui | windowSize = size}

keyPressed : KeyCode -> Set KeyCode -> Bool
keyPressed keycode pressedKeys =
    Set.member keycode pressedKeys
