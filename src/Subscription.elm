port module Subscription exposing (Msg(..), subscriptions)

import Browser.Events exposing (onAnimationFrameDelta)
import Json.Decode as Decode
import Model exposing (..)
import Model.Scene exposing (KeyCode)
import Model.Ui exposing (..)
import Task
import Time exposing (Posix)


type Msg
    = ResizeWindow ( Int, Int )
    | Tick Posix
    | TickFloat Float
    | KeyChange Bool KeyCode
    | StartGame
    | TimeSecond Posix
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions { ui } =
    let
        window =
            onResize ResizeWindow

        keys =
            [ keyPressed (KeyChange True)
            , keyRelease (KeyChange False)
            ]

        animation =
            [ onAnimationFrameDelta TickFloat ]

        seconds =
            Time.every 1000 TimeSecond
    in
    (case ui.screen of
        StartScreen ->
            [ window, seconds ]

        PlayScreen ->
            [ window ] ++ keys ++ animation

        GameoverScreen ->
            [ window ] ++ keys
    )
        |> Sub.batch



-- initialWindowSizeCommand : Cmd Msg
-- initialWindowSizeCommand =
--   Task.perform ResizeWindow


keyDecoder : Decode.Decoder KeyCode
keyDecoder =
    Decode.map toDirection Decode.string


toDirection : String -> KeyCode
toDirection string =
    case string of
        "A" ->
            65

        "W" ->
            87

        "D" ->
            68

        "J" ->
            74

        "L" ->
            76

        "I" ->
            73

        _ ->
            0


decodeKey : Decode.Value -> KeyCode
decodeKey val =
    case Decode.decodeValue keyDecoder val of
        Ok dir ->
            dir

        Err _ ->
            0



-- port


port keyDown : (Decode.Value -> msg) -> Sub msg


keyPressed : (KeyCode -> msg) -> Sub msg
keyPressed toMsg =
    keyDown (\val -> toMsg (decodeKey val))


port keyUp : (Decode.Value -> msg) -> Sub msg


keyRelease : (KeyCode -> msg) -> Sub msg
keyRelease toMsg =
    keyUp (\val -> toMsg (decodeKey val))


port windowResize : (Decode.Value -> msg) -> Sub msg


onResize : (( Int, Int ) -> msg) -> Sub msg
onResize toMsg =
    windowResize (\val -> toMsg (decodeTuple val))


type alias Temp =
    { height : Int
    , width : Int
    }


tupleDecoder : Decode.Decoder Temp
tupleDecoder =
    Decode.map2 Temp
        (Decode.field "height" Decode.int)
        (Decode.field "height" Decode.int)


decodeTuple : Decode.Value -> ( Int, Int )
decodeTuple val =
    let
        y =
            Decode.decodeValue tupleDecoder val

        tuple =
            case y of
                Ok x ->
                    ( x.height, x.width )

                Err _ ->
                    ( 0, 0 )
    in
    tuple
