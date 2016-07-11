module Model.Ui exposing (..)

import Model.Shared exposing (..)


type alias UIState =
  { windowSize : (Int, Int) }

initialUi : UIState
initialUi =
  { windowSize = (500,500) }
