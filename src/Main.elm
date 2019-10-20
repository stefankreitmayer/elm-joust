module Main exposing (..)

import Browser
import Html exposing (Html)
import Model exposing (Model, initialModel)
import Model.Ui exposing (setWindowSize)
import Subscription exposing (subscriptions, decodeTuple)
import Update exposing (update)
import View exposing (view)
import Json.Decode exposing (Value)



--------------------------------------------------------------------------- MAIN


init : Value -> (Model, Cmd msg)
init flag = 
  let
      size = 
          decodeTuple flag 
      
      newModel = 
        {initialModel | ui = setWindowSize size initialModel.ui}
          
  in
  (newModel, Cmd.none)

main : Program Value Model Subscription.Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
