module View exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Svg exposing (Svg,Attribute)
import Svg.Attributes as Attributes exposing (x,y,width,height,fill,fontFamily,textAnchor)
import Svg.Events exposing (onClick)
import Time exposing (Time)
import String

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Subscription exposing (..)

import VirtualDom
import Json.Encode as Json


view : Model -> Html Msg
view {ui,scene,secondsPassed} =
  case ui.screen of
    StartScreen ->
      renderStartScreen ui.windowSize secondsPassed

    PlayScreen ->
      renderPlayScreen ui.windowSize scene

    GameoverScreen ->
      renderGameoverScreen ui.windowSize scene


renderStartScreen : (Int,Int) -> Int -> Html.Html Msg
renderStartScreen (w,h) secondsPassed =
  let
      clickHandler = onClick StartGame
      screenAttrs = [ clickHandler ] ++ (svgAttributes (w,h))
      title = largeText w h (h//5) "Elm Joust"
      clickToStart = smallText w h (h*4//5) "Click to start"
      paragraph y lines = renderTextParagraph (w//2) y (normalFontSize w h) "middle" lines []
      keys = paragraph (h*3//8) [ "Player 1 keys: A W D" , "Player 2 keys: J I L" ]
      goal = paragraph (h*4//8) [ "Get points for pushing", "the other off the edge" ]
      win  = paragraph (h*5//8) [ "Score "++ (toString winScore) ++" points to win!" ]
      githubLink = renderGithubLink (w,h)
      children = [ title, githubLink ]
                 ++ (if secondsPassed >= 1 then [ keys, goal, win ] else [] )
                 ++ (if secondsPassed >= 3 && secondsPassed%2 == 1 then [ clickToStart ] else [] )
  in
      Svg.svg
        screenAttrs
        children


renderGithubLink : (Int,Int) -> Svg Msg
renderGithubLink (w,h) =
  let
      text = "Source code at GitHub"
      line = renderTextLine (w//2) (h*95//100) (normalFontSize w h) "middle" text []
      url = "https://github.com/stefankreitmayer/elm-joust"
  in
      svgHyperlink url line


svgHyperlink : String -> Svg Msg -> Svg Msg
svgHyperlink url child =
  Svg.a
  [ Attributes.xlinkHref url ]
  [ child ]


renderPlayScreen : (Int,Int) -> Scene -> Html.Html Msg
renderPlayScreen (w,h) ({t,player1,player2} as scene) =
  let
      windowSize = (w,h)
  in
     Svg.svg (svgAttributes windowSize)
     [ renderScores windowSize player1.score player2.score
     , renderIce windowSize
     , renderPlayer windowSize player1
     , renderPlayer windowSize player2
     ]


renderGameoverScreen : (Int,Int) -> Scene -> Html.Html Msg
renderGameoverScreen (w,h) {player1,player2} =
  let
      winnerMessage =
        if player1.score>=winScore && player2.score>=winScore then
          "It's a draw"
        else if player1.score>=winScore then
          "Player 1 wins!"
        else
          "Player 2 wins!"
      winnerText = largeText w h (h//3) winnerMessage
      restartText = smallText w h (h//2) "Press SPACE to restart"
      players =
        [ player1, player2 ]
        |> List.filter (\p -> p.score >= winScore)
        |> List.map (\p -> renderPlayer (w,h) p)
      children = [ winnerText , restartText , renderIce (w,h) ] ++ players
  in
      Svg.svg
        (svgAttributes (w,h))
        children


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


renderScores : (Int,Int) -> Int -> Int -> Svg Msg
renderScores (w,h) p1score p2score =
  let
      txt = (toString p1score) ++ "  :  " ++ (toString p2score)
  in
      renderTextLine (w//2) (h//5) ((normalFontSize w h)*2) "middle" txt []


softWhite : String
softWhite = "rgba(255,255,255,.5)"


mediumWhite : String
mediumWhite = "rgba(255,255,255,.8)"


normalFontFamily : String
normalFontFamily =
  "Courier New, Courier, Monaco, monospace"


normalFontSize : Int -> Int -> Int
normalFontSize w h =
  (min w h) // 20 |> min 24


normalLineHeight : Int -> Int -> Int
normalLineHeight w h =
  (toFloat (normalFontSize w h)) * 1.38 |> floor


largeText : Int -> Int -> Int -> String -> Svg Msg
largeText w h y str =
  renderTextLine (w//2) y ((normalFontSize w h)*2) "middle" str []


smallText : Int -> Int -> Int -> String -> Svg Msg
smallText w h y str =
  renderTextLine (w//2) y (normalFontSize w h) "middle" str []


renderTextParagraph : Int -> Int -> Int -> String -> List String -> List (Svg.Attribute Msg) -> Svg Msg
renderTextParagraph xPos yPos fontSize anchor lines extraAttrs =
  List.indexedMap (\index line -> renderTextLine xPos (yPos+index*fontSize*5//4) fontSize anchor line extraAttrs) lines
  |> Svg.g []


renderTextLine : Int -> Int -> Int -> String -> String -> List (Svg.Attribute Msg) -> Svg Msg
renderTextLine xPos yPos fontSize anchor content extraAttrs =
  let
      attributes = [ x <| toString xPos
                   , y <| toString yPos
                   , textAnchor anchor
                   , fontFamily normalFontFamily
                   , Attributes.fontSize (toString fontSize)
                   , fill mediumWhite
                   ]
                   |> List.append extraAttrs
  in
      Svg.text' attributes [ Svg.text content ]
