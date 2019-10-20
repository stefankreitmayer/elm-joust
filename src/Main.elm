module Main exposing (..)

import Browser
import Html exposing (Html)
import Model exposing (Model, initialModel)
import Subscription exposing (subscriptions)
import Update exposing (update)
import View exposing (view)



--------------------------------------------------------------------------- MAIN


main : Program () Model Subscription.Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
