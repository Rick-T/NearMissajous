module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Att exposing (style)
import Plot exposing (..)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { currentTime : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | currentTime = time }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 30 Tick



-- VIEW


myStyle : Attribute msg
myStyle =
    style "height" "90px"


view : Model -> Html Msg
view model =
    let
        plotSettings =
            { defaultSeriesPlotCustomizations | width = 800, height = 600, toDomainLowest = min -5, toRangeLowest = min -5, toDomainHighest = max 5, toRangeHighest = max 5 }
    in
    div [ myStyle ]
        [ text
            (Debug.toString model)
        , Plot.viewSeriesCustom plotSettings
            [ Plot.dots missajouPointData
            , Plot.dots leftCirclePointData
            , Plot.dots rightCirclePointData
            , Plot.line (\m -> missajouPointData m ++ rightCirclePointData m)
            , Plot.line (\m -> missajouPointData m ++ leftCirclePointData m)
            , Plot.line rightCircleCurveData
            , Plot.line leftCircleCurveData
            , Plot.line missajouCurveData
            ]
            model
        ]


posixToSeconds : Time.Posix -> Float
posixToSeconds posix =
    0.001 * (Time.posixToMillis posix |> toFloat)


timePointPlot : (Float -> DataPoint msg) -> Model -> List (DataPoint msg)
timePointPlot generator model =
    let
        t =
            posixToSeconds model.currentTime
    in
    [ generator t ]


timeSeriesPlot : Float -> (Float -> DataPoint msg) -> Model -> List (DataPoint msg)
timeSeriesPlot maxTime generator model =
    let
        timesteps =
            List.map (\i -> maxTime * toFloat i / toFloat numPoints) (List.range 0 numPoints)
    in
    List.map generator timesteps


missajouPointData : Model -> List (DataPoint msg)
missajouPointData =
    timePointPlot
        (\t ->
            Plot.circle (x t) (y t)
        )


leftCirclePointData : Model -> List (DataPoint msg)
leftCirclePointData =
    timePointPlot (\t -> Plot.circle (p1 t) (q1 t))


rightCirclePointData : Model -> List (DataPoint msg)
rightCirclePointData =
    timePointPlot (\t -> Plot.circle (p2 t) (q2 t))


missajouCurveData : Model -> List (DataPoint msg)
missajouCurveData model =
    let
        offset =
            posixToSeconds model.currentTime - 0.5 * previewTime
    in
    timeSeriesPlot previewTime (\t -> Plot.clear (x (t + offset)) (y (t + offset))) model


leftCircleCurveData : Model -> List (DataPoint msg)
leftCircleCurveData =
    let
        maxTime =
            2.0 * pi / omega1
    in
    timeSeriesPlot maxTime
        (\t -> Plot.clear (p1 t) (q1 t))


rightCircleCurveData : Model -> List (DataPoint msg)
rightCircleCurveData =
    let
        maxTime =
            2 * pi / omega2
    in
    timeSeriesPlot maxTime
        (\t -> Plot.clear (p2 t) (q2 t))


numPoints : Int
numPoints =
    500


previewTime : Float
previewTime =
    20.0



-- CONSTANTS


l1 : Float
l1 =
    2.5


l2 : Float
l2 =
    2.5


omega1 : Float
omega1 =
    1.0


omega2 : Float
omega2 =
    2.01


r1 : Float
r1 =
    1.0


r2 : Float
r2 =
    1.0


circleDistance : Float
circleDistance =
    3.0



-- DERIVED VALUES


alpha : Float -> Float
alpha t =
    omega1 * t


beta : Float -> Float
beta t =
    omega2 * t


p1 : Float -> Float
p1 t =
    r1 * sin (alpha t)


p2 : Float -> Float
p2 t =
    circleDistance + r2 * sin (beta t)


q1 : Float -> Float
q1 t =
    r1 * cos (alpha t)


q2 : Float -> Float
q2 t =
    r2 * cos (beta t)


square : Float -> Float
square x_ =
    x_ * x_


l3squared : Float -> Float
l3squared t =
    square (p2 t - p1 t) + square (q2 t - q1 t)


l3 : Float -> Float
l3 =
    sqrt << l3squared


cosTheta : Float -> Float
cosTheta t =
    (square l1 + l3squared t - square l2) / (2 * l1 * l3 t)


h : Float -> Float
h t =
    sqrt (square l1 - square (rho t))


rho : Float -> Float
rho t =
    l1 * cosTheta t


x : Float -> Float
x t =
    p1 t + l1 / l3 t * (p2 t - p1 t) * cosTheta t - h t / l3 t * (q2 t - q1 t)


y : Float -> Float
y t =
    q1 t + l1 / l3 t * (q2 t - q1 t) * cosTheta t + h t / l3 t * (p2 t - p1 t)
