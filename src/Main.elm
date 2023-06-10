module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp, onAnimationFrameDelta)
import Html exposing (Html, div)
import Json.Decode as Json exposing (Decoder, field, string)
import Svg exposing (Svg, svg, rect, circle, polygon)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, cx, cy, r, points, fill, stroke)

import Vector exposing (..)
import Input



-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = \_->( initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model =
  { ball : MovingPoint
  , lFlipper : Flipper
  , rFlipper : Flipper
  , fixedPoints : List FixedPointCollider
  , input : Input.KeySet
  }

updateBall : (MovingPoint -> MovingPoint) -> Model -> Model
updateBall updater model =
  { model | ball = updater model.ball }

updateInput : (Input.KeySet -> Input.KeySet) -> Model -> Model
updateInput updater model =
  { model | input = updater model.input }

updateLFlipper : (Flipper -> Flipper) -> Model -> Model
updateLFlipper updater model =
  { model | lFlipper = updater model.lFlipper }

updateRFlipper : (Flipper -> Flipper) -> Model -> Model
updateRFlipper updater model =
  { model | rFlipper = updater model.rFlipper }

type alias FixedPointCollider =
  { p : Vector
  , r : Float
  }

type alias MovingPointCollider =
  { p : Vector
  , v : Vector
  , r : Float
  }

type alias MovingPoint =
  { p : Vector
  , v : Vector
  }

toMoving : Vector -> MovingPoint
toMoving vec =
  { p = vec
  , v = zero
  }

type alias Wall = List Vector

type alias Pin = Vector

type alias Flipper =
  { p : Vector
  , collisions : List Vector
  , theta : Float
  , lastTheta : Float
  , thetaMin : Float
  , thetaMax : Float
  , omega : Float
  }

type alias Matrix =
  { a00 : Float, a10 : Float
  , a01 : Float, a11 : Float
  }

mulm : Matrix -> Vector -> Vector
mulm  {a00, a10, a01, a11} {x, y} =
  { x = x * a00 + y * a10
  , y = x * a01 + y * a11
  }

rotate : Float -> Matrix
rotate theta =
  { a00 = cos theta, a10 = -(sin theta)
  , a01 = sin theta, a11 =   cos theta
  }

gravity =
  { x = 0.0
  , y = 1.5e-4
  }

ballRarius = 7.0

wallRebound = 0.6
flipperRebound = 0.85

flipperSpeed = 0.012

initialModel =
  { ball =
    { p =
      { x = 100.0
      , y = 50.0
      }
    , v =
      { x = 0.1
      , y = 0.0
      }
    }
  , lFlipper =
    { p =
      { x = 80.0, y = 290.0 }
    , theta = 0.0
    , lastTheta = 0.0
    , thetaMin = -0.5
    , thetaMax = 0.5
    , omega = flipperSpeed
    , collisions = flipperShape
    }
  , rFlipper =
    { p =
      { x = 160.0, y = 290.0 }
    , theta = 3.14
    , lastTheta = 3.14
    , thetaMin = 2.64
    , thetaMax = 3.64
    , omega = -flipperSpeed
    , collisions = flipperShape
    }
  , fixedPoints =
    [ arc { x = 120.0, y = 110.0 } 100.0 (2.44, 6.98) 0.05
    , arc { x = 120.0, y = 110.0 }  80.0 (2.64, 6.78) 0.06

    , line { x =  43.0, y = 175.0 } { x =  43.0, y = 210.0 } 5.0
    , line { x =  43.0, y = 210.0 } { x =  20.0, y = 230.0 } 5.0
    , line { x =  20.0, y = 230.0 } { x =  20.0, y = 320.0 } 5.0
    , line { x =  43.0, y = 235.0 } { x =  43.0, y = 270.0 } 5.0
    , line { x =  43.0, y = 270.0 } { x =  80.0, y = 285.0 } 5.0

    , line { x = 197.0, y = 175.0 } { x = 197.0, y = 210.0 } 5.0
    , line { x = 197.0, y = 210.0 } { x = 220.0, y = 230.0 } 5.0
    , line { x = 220.0, y = 230.0 } { x = 220.0, y = 320.0 } 5.0
    , line { x = 197.0, y = 235.0 } { x = 197.0, y = 270.0 } 5.0
    , line { x = 197.0, y = 270.0 } { x = 160.0, y = 285.0 } 5.0
    ]
      |> List.concat
      |> List.map (\p -> { p = p, r = wallRebound })
  , input = Input.empty
  }

flipperShape =
     line { x = 0.0, y = -5.0 } { x = 25.0, y =  0.0 } 4.0
  ++ line { x = 0.0, y =  5.0 } { x = 25.0, y =  0.0 } 4.0

line start end gap =
  let
    help start_ end_ list =
      if dst2 start_ end_ < gap*gap
      then
        end_ :: list
      else
        let
          start_dir = normalize (sub start_ end_)
          end_1 = add end_ (mul gap start_dir)
        in
          help start_ end_1 (end_ :: list)
  in
    start :: help start end []

arc center radius (start, end) gap =
  let
    help start_ end_ list =
      let
        v =
          add
          center
          (mulm (rotate end_) { x = radius, y = 0 })
      in
        if end_ - start_ < gap
        then v :: list
        else help start_ (end_ - gap) (v :: list)
    u =
      add
      center
      (mulm (rotate start) { x = radius, y = 0 })
  in
    u :: help start end []



-- UPDATE

type Msg
  = Tick Float
  | InputUpdate Input.KeySet
  | None

type Input
  = LFlipper UpDown
  | RFlipper UpDown

type UpDown
  = Up
  | Down

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      InputUpdate status ->
        let
          omegaUpdater o f =
            { f | omega = o }

          lFlipper =
            model.lFlipper
              |> omegaUpdater (if Input.member Input.LFlipper status then -flipperSpeed else flipperSpeed)
          
          rFlipper =
            model.rFlipper
              |> omegaUpdater (if Input.member Input.RFlipper status then flipperSpeed else -flipperSpeed)

          model_ =
            model
              |> updateInput (\_ -> status)
              |> updateLFlipper (\_ -> lFlipper)
              |> updateRFlipper (\_ -> rFlipper)

        in
          ( model_
          , Cmd.none
          )

      Tick delta ->
        let
          model_ = model |> updatePinball delta 0
        in
          ( model_, Cmd.none )

      _ ->
        ( model, Cmd.none )

type alias ColliderInfo =
  { collision : MovingPoint
  , reaction : Model -> Model
  }

rebound : Float -> MovingPoint -> (Model -> Model)
rebound reb target model =
  let
    ball = model.ball
    pb = ball.p
    vb = ball.v
    pt = target.p
    vt = target.v
    vr = sub vb vt

    pr = sub pb pt
    prn = normalize pr
    impact = mul (dot prn vr * (-2 * reb)) prn
    vb_ = add vb impact

    pb_ =
      if dst2 pb pt > ballRarius * ballRarius
      then pb
      else add pt (mul ballRarius prn)

    ball_ = { ball | p = pb_, v = vb_ }
  in
    { model | ball = ball_ }

updatePinball : Float -> Int -> Model -> Model
updatePinball maxElapse updateCount model =
  if maxElapse <= 0.0
  then model
  else if updateCount == 2
  then
    through maxElapse model
  else
    let
      ( impactTime, collisionEffect ) =
        getCollisionCandidates model
        -- TODO フリッパーと小さい相対速度で衝突したとき貫通する場合への対処
          |> List.filter (\c -> closing c.collision model.ball) 
          |> List.map (predictImpact model.ball)
          |> List.filter (\( f, _ ) -> f > 0.0 )
          |> minBy Tuple.first ( maxElapse, (\m -> m) )

      model_ =
        model
          |> through impactTime
          |> collisionEffect
    in
      updatePinball
      (maxElapse - impactTime)
      (updateCount + 1)
      model_

through : Float -> Model -> Model
through elapsed =
  updateBall (freeFall elapsed)
    >> updateLFlipper (flipperUpdater elapsed)
    >> updateRFlipper (flipperUpdater elapsed)

freeFall : Float -> MovingPoint -> MovingPoint
freeFall elapsed { p, v } =
  let
    p_= add p (mul elapsed v)
    v_= add v (mul elapsed gravity)
  in
    { p = p_, v = v_ }

flipperUpdater : Float -> Flipper -> Flipper
flipperUpdater elapsed flipper =
  let
    theta = clamp flipper.thetaMin flipper.thetaMax (flipper.theta + flipper.omega * elapsed)
  in
    { flipper | theta = theta }

getCollisionCandidates : Model -> List ColliderInfo
getCollisionCandidates model =
  let
    ball = model.ball

    vsFixed =
      model.fixedPoints
        |> List.map (\fixed ->
          { collision = { p = fixed.p, v = zero }
          , reaction = rebound fixed.r { p = fixed.p, v = zero }
          })

    vsRFlipper =
      flipperToMovingPointCollider model.rFlipper
        |> List.map (\mp ->
          { collision = mp
          , reaction = rebound flipperRebound mp
          })

    vsLFlipper =
      flipperToMovingPointCollider model.lFlipper
        |> List.map (\mp ->
          { collision = mp
          , reaction = rebound flipperRebound mp
          })
  in
    vsFixed ++ vsRFlipper ++ vsLFlipper

closing : MovingPoint -> MovingPoint -> Bool
closing p1 p2 =
  Vector.dot
    (Vector.sub p1.p p2.p)
    (Vector.sub p1.v p2.v)
  < 0.0

predictImpact : MovingPoint -> ColliderInfo -> ( Float, Model -> Model )
predictImpact ball collider =
  ( getImpactTime ball collider.collision
  , collider.reaction
  )

-- ボールの衝突までの時間を返す．衝突しない場合はNaN．前提条件：ボールが接近中である 
getImpactTime : MovingPoint -> MovingPoint -> Float
getImpactTime target ball =
  let
    pb = ball.p
    vb = ball.v
    pt = target.p
    vt = target.v

    pr = sub pb pt
    vr = sub vb vt

    -- t経過後のballの相対位置 : pr + vr t
    -- その時のtargetからの距離 : vr^2 t^2 + 2 pr vr t + pr^2 - ballRarius^2

    a = dot vr vr
    b = 2 * dot pr vr
    c = dot pr pr - ballRarius * ballRarius

  in
    -2 * c / (b - sqrt(b*b-4*a*c))

flipperToMovingPointCollider flipper =
  let
    matrixTheta =
      rotate flipper.theta
    matrixL90 =
      { a00 = 0.0, a10 = -1.0
      , a01 = 1.0, a11 =  0.0
      }
  in
    flipper.collisions
      |> List.map (\p ->
        { p = add (mulm matrixTheta p) flipper.p
        , v = mul (flipper.omega / 100.0) (mulm matrixL90 (sub p flipper.p))
        })



-- VIEW

view : Model -> Html Msg
view model =
  div
  []
  [ svg
    [ width "480"
    , height "640"
    , viewBox "0 0 240 320"
    ]
    ( List.concat
      [ model.ball        |> ballView
      , model.lFlipper    |> flipperView
      , model.rFlipper    |> flipperView
      , model.fixedPoints |> List.map fixedPointColliderView |> List.concat
      ]
    )
  ]

ballView : MovingPoint -> List (Svg msg)
ballView ball =
  [ circle
    [ cx <| String.fromFloat ball.p.x
    , cy <| String.fromFloat ball.p.y
    , r  <| String.fromFloat ballRarius
    , fill "red"
    ]
    []
  ]

flipperView : Flipper -> List (Svg msg)
flipperView flipper =
  let
    matrix =
      rotate flipper.theta
    collisions =
      flipper.collisions
        |> List.map (\v ->
          add (mulm matrix v) flipper.p)
        |> List.map (\p ->
          { p = p, r = flipperRebound })
  in
    List.map fixedPointColliderView collisions
      |> List.concat

fixedPointColliderView : FixedPointCollider -> List (Svg msg)
fixedPointColliderView fixed =
  [ circle
    [ cx <| String.fromFloat fixed.p.x
    , cy <| String.fromFloat fixed.p.y
    , r  <| "2"
    , fill <| reboundToColor fixed.r
    ]
    []
  ]

reboundToColor r =
  if r > 0.91 then "#D00"
  else if r > 0.81 then "#F00"
  else if r > 0.71 then "#F80"
  else if r > 0.61 then "#880"
  else if r > 0.51 then "#8F0"
  else if r > 0.41 then "#0F0"
  else if r > 0.31 then "#0F8"
  else if r > 0.21 then "#088"
  else if r > 0.11 then "#00F"
  else "#008"



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Input.decoder InputUpdate keyConfig model.input
    , onAnimationFrameDelta Tick
    ]

keyConfig : String -> Maybe Input.Key 
keyConfig str =
  case str of
    "z" -> Just Input.LFlipper
    "." -> Just Input.RFlipper
    _ -> Nothing



-- UTILYTY

minBy : (a -> comparable) -> a -> List a -> a
minBy extractor default list =
  let
    help a min =
      let
        (mina, minc) = min
        ac = extractor a
      in
        if ac < minc
        then (a, ac)
        else min
  in
    list
      |> List.foldl help ( default, extractor default )
      |> Tuple.first
