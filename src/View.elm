module View exposing (view)

import Html exposing (Html)
import Svg exposing (Svg,Attribute)
import Svg.Attributes as Attributes exposing (x,y,width,height,fill)
import Svg.Events exposing (onClick)
import Time exposing (Time)
import String

import Model exposing (..)
import Model.Shared exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Subscription exposing (..)

import VirtualDom
import Json.Encode as Json


view : Model -> Html Msg
view {ui,scene} =
  renderAll ui.windowSize scene


renderAll : (Int,Int) -> Scene -> Html.Html Msg
renderAll (w,h) ({t,player} as scene) =
  let
      windowSize = (w,h)
  in
     Svg.svg (svgAttributes windowSize)
     [ renderIce windowSize
     , renderPlayer windowSize player
     ]


svgAttributes : (Int, Int) -> List (Attribute Msg)
svgAttributes (w, h) =
  [ width (toString w)
  , height (toString h)
  , Attributes.viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
  , VirtualDom.property "xmlns:xlink" (Json.string "http://www.w3.org/1999/xlink")
  , Attributes.version "1.1"
  , Attributes.style "position: fixed;"
  ]


renderIce : (Int,Int) -> Svg Msg
renderIce (w,h) =
  let
      xString = (toFloat w) * icePosX |> toString
      yString = (toFloat (h-w)) + (toFloat w) * icePosY |> toString
      widthString = (toFloat w) * iceWidth |> toString
      heightString = (toFloat w) * (1-icePosY) |> toString
  in
      Svg.rect
        [ x xString
        , y yString
        , width widthString
        , height heightString
        , fill softWhite
        ]
        []


renderPlayer : (Int,Int) -> Player -> Svg Msg
renderPlayer (w,h) {position} =
  let
      x = (toFloat w) * position.x |> toString
      y = (toFloat (h-w)) + (toFloat w) * (position.y-playerRadius) |> toString
      radius = (toFloat w) * playerRadius |> toString
  in
      Svg.circle
        [ Attributes.cx x
        , Attributes.cy y
        , Attributes.r radius
        , fill softWhite
        ]
        []


softWhite : String
softWhite = "rgba(255,255,255,.2)"
