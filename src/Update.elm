module Update exposing (..)

import Model exposing (..)
import Model.Scene exposing (..)
import Subscription exposing (..)

------------------------------------------------------------------------- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update action ({ui,scene} as model) =
  case action of
    ResizeWindow dimensions ->
      let
          points' = createPoints dimensions
      in
         ({ model | ui = { ui | windowSize = dimensions }
                              , scene = { scene | points = points' } }, Cmd.none)

    Tick time ->
      let
          points' = scene.points |> movePoints time
          scene' = { scene | t = time, points = points' }
      in
         if time > model.lastFrameTime+40 then
           ({ model | scene = scene', lastFrameTime = time }, Cmd.none)
         else
           (model, Cmd.none)

    NoOp ->
      (model, Cmd.none)


createPoints : (Int,Int) -> List Point
createPoints dimensions =
  let
      n = 2
  in
     List.map (\i -> createPoint dimensions i n) [ 0..n ]


createPoint : (Int,Int) -> Int -> Int -> Point
createPoint (w,h) i nPoints =
  let
      x = (toFloat i)/(toFloat nPoints)*(toFloat w)
      y = h//3+(i%2*h//20) |> toFloat
  in
      (x,y)


movePoints : Float -> List Point -> List Point
movePoints t points =
  List.map (movePoint t) points


movePoint : Float -> Point -> Point
movePoint t (x,y) =
  (x, (240 + (sin (t/1000 + x/100)) * 50)  )
