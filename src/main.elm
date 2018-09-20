module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Att exposing (class)
import Html.Events exposing (onInput)
import Plot exposing (..)
import Round exposing (round)
import Svg.Attributes exposing (stroke)
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
    , omega1 : Float
    , omega2 : Float
    , alpha : Float
    , beta : Float
    , circleDistance : Float
    , t : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0) 2.5 2.5 1.0 1.0 1.0 2.01 0.0 0.0 3.0 0.0
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Update Parameter String


type Parameter
    = L1
    | L2
    | R1
    | R2
    | Omega1
    | Omega2
    | Distance


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                next =
                    step 1 model
            in
            ( { next | currentTime = time }
            , Cmd.none
            )

        Update p str ->
            ( updateParameter p str model, Cmd.none )


updateParameter : Parameter -> String -> Model -> Model
updateParameter param str model =
    case param of
        L1 ->
            { model | l1 = String.toFloat str |> Maybe.withDefault model.l1 }

        L2 ->
            { model | l2 = String.toFloat str |> Maybe.withDefault model.l2 }

        R1 ->
            { model | r1 = String.toFloat str |> Maybe.withDefault model.r1 }

        R2 ->
            { model | r2 = String.toFloat str |> Maybe.withDefault model.r2 }

        Omega1 ->
            { model | omega1 = String.toFloat str |> Maybe.withDefault model.omega1 }

        Omega2 ->
            { model | omega2 = String.toFloat str |> Maybe.withDefault model.omega2 }

        Distance ->
            { model | circleDistance = String.toFloat str |> Maybe.withDefault model.circleDistance }


shift : Float -> Model -> Model
shift secondsPassed model =
    { model | t = model.t + secondsPassed, alpha = model.alpha + model.omega1 * secondsPassed, beta = model.beta + model.omega2 * secondsPassed }


step : Int -> Model -> Model
step i =
    let
        secondsPassed =
            0.001 * tickrate * toFloat i
    in
    shift secondsPassed



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every tickrate Tick



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ viewSliderWithLabel L1 model
            , viewSliderWithLabel L2 model
            , viewSliderWithLabel R1 model
            , viewSliderWithLabel R2 model
            , viewSliderWithLabel Omega1 model
            , viewSliderWithLabel Omega2 model
            , viewSliderWithLabel Distance model
            ]
        , div [ class "row" ]
            [ viewPlot model
            ]
        ]


viewPlot : Model -> Html Msg
viewPlot model =
    let
        default =
            defaultSeriesPlotCustomizations

        maxDist =
            0.5 * (model.r1 + model.l1 + model.r2 + model.l2)

        plotSettings =
            { default | width = 600, height = 400, toDomainLowest = \i -> min -model.r1 -model.r2, toRangeLowest = \i -> min -5 -maxDist, toDomainHighest = \i -> 1.4 * max 5 maxDist, toRangeHighest = \i -> max 5 maxDist }
    in
    div [ class "plot" ]
        [ Plot.viewSeriesCustom plotSettings
            [ Plot.line (\m -> missajouPointData "lightblue" m ++ rightCirclePointData "black" m) |> colorSeries "green"
            , Plot.line (\m -> missajouPointData "lightblue" m ++ leftCirclePointData "black" m) |> colorSeries "green"
            , Plot.line rightCircleCurveData |> colorSeries "black"
            , Plot.line leftCircleCurveData |> colorSeries "black"
            , Plot.line missajouCurveData |> colorSeries "pink"
            ]
            model
        ]


colorSeries : String -> Series data msg -> Series data msg
colorSeries color series =
    case
        series.interpolation
    of
        None ->
            series

        Linear fill atts ->
            { series | interpolation = Linear fill [ stroke color ] }

        Monotone fill atts ->
            { series | interpolation = Monotone fill [ stroke color ] }


viewSliderWithLabel : Parameter -> Model -> Html Msg
viewSliderWithLabel p m =
    div [ class "slider-with-label", class "col", class "form-group" ] [ viewLabel p m, viewSlider p m ]


viewLabel : Parameter -> Model -> Html Msg
viewLabel param model =
    label [ class "label", class "form-label", class "text-center" ] [ text <| nameOf param ++ ": " ++ (extract param model |> round 2) ]


viewSlider : Parameter -> Model -> Html Msg
viewSlider param model =
    div [ class "form-inline", class "row" ]
        [ label [ class "form-label", class "form-inline" ] [ minOf param model |> round 2 |> text ]
        , input
            [ Att.type_ "range"
            , Att.min <| String.fromFloat <| minOf param model
            , Att.max <| String.fromFloat <| maxOf param model
            , Att.value <| String.fromFloat <| extract param model
            , Att.step "0.0001"
            , class "range"
            , onInput <| Update param
            ]
            []
        , label [] [ maxOf param model |> round 2 |> text ]
        ]


nameOf : Parameter -> String
nameOf p =
    case p of
        L1 ->
            "L1"

        L2 ->
            "L2"

        R1 ->
            "R1"

        R2 ->
            "R2"

        Omega1 ->
            "Omega1"

        Omega2 ->
            "Omega2"

        Distance ->
            "Distance"


extract : Parameter -> (Model -> Float)
extract param =
    case param of
        L1 ->
            .l1

        L2 ->
            .l2

        R1 ->
            .r1

        R2 ->
            .r2

        Omega1 ->
            .omega1

        Omega2 ->
            .omega2

        Distance ->
            .circleDistance


maxOf : Parameter -> (Model -> Float)
maxOf param =
    case param of
        L1 ->
            l1max

        L2 ->
            l2max

        R1 ->
            r1max

        R2 ->
            r2max

        Omega1 ->
            \i -> omegaMax

        Omega2 ->
            \i -> omegaMax

        Distance ->
            dmax


minOf : Parameter -> (Model -> Float)
minOf param =
    case param of
        L1 ->
            l1min

        L2 ->
            l2min

        R1 ->
            \i -> 0

        R2 ->
            \i -> 0

        Omega1 ->
            \i -> 0

        Omega2 ->
            \i -> 0

        Distance ->
            dmin


l1min : Model -> Float
l1min m =
    abs (m.l2 - m.circleDistance) + (m.r1 + m.r2)


l1max : Model -> Float
l1max m =
    m.l2 + m.circleDistance - m.r1 - m.r2


l2min : Model -> Float
l2min m =
    abs (m.l1 - m.circleDistance) + (m.r1 + m.r2)


l2max : Model -> Float
l2max m =
    m.l1 + m.circleDistance - m.r1 - m.r2


r1max : Model -> Float
r1max m =
    min (m.l1 - m.r2 - abs (m.circleDistance - m.l2)) (m.circleDistance + m.l2 - m.l1 - m.r2)


r2max : Model -> Float
r2max m =
    min (m.l1 - m.r1 - abs (m.circleDistance - m.l2)) (m.circleDistance + m.l2 - m.l1 - m.r1)


dmin : Model -> Float
dmin m =
    abs (m.l1 - m.l2) + m.r1 + m.r2


dmax : Model -> Float
dmax m =
    m.l1 + m.l2 - m.r1 - m.r2


posixToSeconds : Time.Posix -> Float
posixToSeconds posix =
    0.001 * (Time.posixToMillis posix |> toFloat)


timePointPlot : String -> Float -> Float -> List (DataPoint msg)
timePointPlot color x_ y_ =
    [ Plot.dot (Plot.viewCircle 5.0 color) x_ y_ ]


timeSeriesPlot : Float -> ( Model -> Float, Model -> Float ) -> Model -> List (DataPoint msg)
timeSeriesPlot maxTime ( x_, y_ ) model =
    let
        timesteps =
            List.map (\i -> maxTime * toFloat i / toFloat numPoints) (List.range 0 numPoints)

        models =
            List.map shift timesteps
    in
    List.map (\m -> Plot.clear (x_ m) (y_ m)) (List.map (\f -> f model) models)


missajouPointData : String -> Model -> List (DataPoint msg)
missajouPointData color model =
    timePointPlot color (x model) (y model)


leftCirclePointData : String -> Model -> List (DataPoint msg)
leftCirclePointData color model =
    timePointPlot color (p1 model) (q1 model)


rightCirclePointData : String -> Model -> List (DataPoint msg)
rightCirclePointData color model =
    timePointPlot color (p2 model) (q2 model)


missajouCurveData : Model -> List (DataPoint msg)
missajouCurveData model =
    let
        offset =
            -0.5 * previewTime

        offsetModel =
            { model | t = model.t + offset }
    in
    timeSeriesPlot previewTime ( x, y ) offsetModel


leftCircleCurveData : Model -> List (DataPoint msg)
leftCircleCurveData model =
    let
        maxTime =
            2.0 * pi / model.omega1
    in
    timeSeriesPlot maxTime ( p1, q1 ) model


rightCircleCurveData : Model -> List (DataPoint msg)
rightCircleCurveData model =
    let
        maxTime =
            2 * pi / model.omega2
    in
    timeSeriesPlot maxTime ( p2, q2 ) model



-- CONSTANTS


omegaMax : Float
omegaMax =
    5.0


numPoints : Int
numPoints =
    500


previewTime : Float
previewTime =
    20.0


tickrate : Float
tickrate =
    30



-- DERIVED VALUES


p1 : Model -> Float
p1 m =
    m.r1 * sin m.alpha - 0.5 * m.circleDistance


p2 : Model -> Float
p2 m =
    0.5 * m.circleDistance + m.r2 * sin m.beta


q1 : Model -> Float
q1 m =
    m.r1 * cos m.alpha


q2 : Model -> Float
q2 m =
    m.r2 * cos m.beta


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
