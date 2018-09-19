module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Att exposing (style)
import Html.Events exposing (onInput)
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
    , l1 : Float
    , l2 : Float
    , r1 : Float
    , r2 : Float
    , t : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0) 2.5 2.5 1.0 1.0 0.0
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Update Parameter


type Parameter
    = L1 String
    | L2 String
    | R1 String
    | R2 String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | currentTime = time, t = model.t + 0.001 * tickrate }
            , Cmd.none
            )

        Update p ->
            ( updateParameter p model, Cmd.none )


updateParameter : Parameter -> Model -> Model
updateParameter param model =
    case param of
        L1 l ->
            { model | l1 = String.toFloat l |> Maybe.withDefault 2.5 }

        L2 l ->
            { model | l2 = String.toFloat l |> Maybe.withDefault 2.5 }

        R1 r ->
            { model | r1 = String.toFloat r |> Maybe.withDefault 1.0 }

        R2 r ->
            { model | r2 = String.toFloat r |> Maybe.withDefault 1.0 }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every tickrate Tick


tickrate : Float
tickrate =
    30



-- VIEW


view : Model -> Html Msg
view model =
    let
        plotSettings =
            { defaultSeriesPlotCustomizations | width = 800, height = 600, toDomainLowest = min -5, toRangeLowest = min -5, toDomainHighest = max 5, toRangeHighest = max 5 }
    in
    div []
        [ div [] [ viewSlider L1 .l1 l1min l1max model, text "L1" ]
        , div [] [ viewSlider L2 .l2 l2min l2max model, text "L2" ]
        , div [] [ viewSlider R1 .r1 r1min r1max model, text "R1" ]
        , div [] [ viewSlider R2 .r2 r2min r2max model, text "R2" ]
        , text
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


viewSlider : (String -> Parameter) -> (Model -> Float) -> (Model -> Float) -> (Model -> Float) -> Model -> Html Msg
viewSlider param extract minval maxval model =
    div []
        [ text <| String.fromFloat <| minval model
        , input
            [ Att.type_ "range"
            , Att.min <| String.fromFloat <| minval model
            , Att.max <| String.fromFloat <| maxval model
            , Att.value <| String.fromFloat <| extract model
            , Att.step "0.01"
            , onInput <| Update << param
            ]
            []
        , text <| String.fromFloat <| maxval model
        ]


l1min : Model -> Float
l1min m =
    max (circleDistance + m.r1 + m.r2 - m.l2) (m.l2 + m.r1 + m.r2 - circleDistance)


l1max : Model -> Float
l1max m =
    m.l2 + circleDistance - m.r1 - m.r2


l2min : Model -> Float
l2min m =
    max (circleDistance + m.r1 + m.r2 - m.l1) (m.l1 + m.r1 + m.r2 - circleDistance)


l2max : Model -> Float
l2max m =
    m.l1 + circleDistance - m.r1 - m.r2


r1min : Model -> Float
r1min m =
    case List.maximum <| r1Constraints m of
        Just f ->
            if f > 0 then
                0.0

            else
                -f

        Nothing ->
            0.0


r1max : Model -> Float
r1max m =
    case List.minimum <| r1Constraints m of
        Just f ->
            f

        Nothing ->
            0.0


r2min : Model -> Float
r2min m =
    case List.maximum <| r2Constraints m of
        Just f ->
            if f > 0 then
                0.0

            else
                -f

        Nothing ->
            0.0


r2max : Model -> Float
r2max m =
    case List.minimum <| r2Constraints m of
        Just f ->
            f

        Nothing ->
            0.0


r1Constraints : Model -> List Float
r1Constraints m =
    List.filter (\i -> i >= 0.0)
        [ m.l1 + m.l2 - circleDistance - m.r2
        ]


r2Constraints : Model -> List Float
r2Constraints m =
    List.filter (\i -> i >= 0.0)
        [ m.l2 + m.l1 - circleDistance - m.r1 ]


posixToSeconds : Time.Posix -> Float
posixToSeconds posix =
    0.001 * (Time.posixToMillis posix |> toFloat)


timePointPlot : (Model -> DataPoint msg) -> Model -> List (DataPoint msg)
timePointPlot generator model =
    [ generator model ]


timeSeriesPlot : Float -> (Model -> DataPoint msg) -> Model -> List (DataPoint msg)
timeSeriesPlot maxTime generator model =
    let
        timesteps =
            List.map (\i -> maxTime * toFloat i / toFloat numPoints) (List.range 0 numPoints)

        models =
            List.map (\t_ -> { model | t = model.t + t_ }) timesteps
    in
    List.map generator models


missajouPointData : Model -> List (DataPoint msg)
missajouPointData model =
    timePointPlot
        (\t ->
            Plot.circle (x model) (y model)
        )
        model


leftCirclePointData : Model -> List (DataPoint msg)
leftCirclePointData =
    timePointPlot (\m -> Plot.circle (p1 m) (q1 m))


rightCirclePointData : Model -> List (DataPoint msg)
rightCirclePointData =
    timePointPlot (\m -> Plot.circle (p2 m) (q2 m))


missajouCurveData : Model -> List (DataPoint msg)
missajouCurveData model =
    let
        offset =
            -0.5 * previewTime

        offsetModel =
            { model | t = model.t + offset }
    in
    timeSeriesPlot previewTime (\m -> Plot.clear (x m) (y m)) offsetModel


leftCircleCurveData : Model -> List (DataPoint msg)
leftCircleCurveData =
    let
        maxTime =
            2.0 * pi / omega1
    in
    timeSeriesPlot maxTime
        (\m -> Plot.clear (p1 m) (q1 m))


rightCircleCurveData : Model -> List (DataPoint msg)
rightCircleCurveData =
    let
        maxTime =
            2 * pi / omega2
    in
    timeSeriesPlot maxTime
        (\m -> Plot.clear (p2 m) (q2 m))


numPoints : Int
numPoints =
    500


previewTime : Float
previewTime =
    20.0



-- CONSTANTS


omega1 : Float
omega1 =
    1.0


omega2 : Float
omega2 =
    2.01


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


p1 : Model -> Float
p1 m =
    m.r1 * sin (alpha m.t)


p2 : Model -> Float
p2 m =
    circleDistance + m.r2 * sin (beta m.t)


q1 : Model -> Float
q1 m =
    m.r1 * cos (alpha m.t)


q2 : Model -> Float
q2 m =
    m.r2 * cos (beta m.t)


square : Float -> Float
square x_ =
    x_ * x_


l3squared : Model -> Float
l3squared m =
    square (p2 m - p1 m) + square (q2 m - q1 m)


l3 : Model -> Float
l3 =
    sqrt << l3squared


cosTheta : Model -> Float
cosTheta m =
    (square m.l1 + l3squared m - square m.l2) / (2 * m.l1 * l3 m)


h : Model -> Float
h m =
    sqrt (square m.l1 - square (rho m))


rho : Model -> Float
rho m =
    m.l1 * cosTheta m


x : Model -> Float
x m =
    p1 m + m.l1 / l3 m * (p2 m - p1 m) * cosTheta m - h m / l3 m * (q2 m - q1 m)


y : Model -> Float
y m =
    q1 m + m.l1 / l3 m * (q2 m - q1 m) * cosTheta m + h m / l3 m * (p2 m - p1 m)
