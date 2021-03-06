module Calc2 exposing (..)

import Browser
import Html exposing (Attribute, Html, div, h2, hr, input, label, option, p, select, span, sub, table, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { userLinearInput : String
    , userLogInput : String
    , selectedInputRange : RangeItem
    , selectedOutputRange : RangeItem
    , selectedPressureScale : PressureScale
    , calculatedRangeValue : String
    , calculatedPressureValue : String
    }


type alias RangeItem =
    { name : String
    , min : Float
    , max : Float
    }


rangeItem : List RangeItem
rangeItem =
    [ RangeItem "0 to 20 mA" 0 20
    , RangeItem "4 to 20 mA" 4 20
    , RangeItem "-10 to 10 VDC" -10 10
    , RangeItem "0 to 10 VDC" 0 10
    , RangeItem "800 to 2500 °C" 800 2500
    , RangeItem "1000 to 2500 °C" 1000 2500
    , RangeItem "0 to 5000 °C" 0 5000
    , RangeItem "1000 to 5000 °C" 1000 5000
    ]


type PressureScale
    = Torr
    | Millibar
    | Pascal


initialModel : Model
initialModel =
    { userLinearInput = "4"
    , userLogInput = "0"
    , selectedInputRange = RangeItem "4 to 20mA" 4 20
    , selectedOutputRange = RangeItem "800 to 2500 °C" 800 2500
    , selectedPressureScale = Torr
    , calculatedRangeValue = "4"
    , calculatedPressureValue = "Voltage..."
    }


init : () -> ( Model, Cmd Msg )
init _ =
    updateLinearScaledValue initialModel



-- UPDATE


type Msg
    = UserLinearInputChange String
    | InputRangeSelected String
    | OutputRangeSelected String
    | UserLogInputChange String
    | SelectedScale PressureScale


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserLinearInputChange value ->
            updateLinearScaledValue { model | userLinearInput = value }

        InputRangeSelected value ->
            updateLinearScaledValue { model | selectedInputRange = getSelectedRangeItem value }

        OutputRangeSelected value ->
            updateLinearScaledValue { model | selectedOutputRange = getSelectedRangeItem value }

        UserLogInputChange value ->
            updateLogScaledValue { model | userLogInput = value }

        SelectedScale value ->
            updateLogScaledValue { model | selectedPressureScale = value }


getSelectedRangeItem : String -> RangeItem
getSelectedRangeItem value =
    case List.filter (\val -> val.name == value) rangeItem |> List.head of
        Nothing ->
            -- need to look at something different here
            RangeItem "4 to 20 mA" 4 20

        Just val ->
            val


onSelectedChange : (String -> msg) -> Attribute msg
onSelectedChange msg =
    on "change" (Decode.map msg Html.Events.targetValue)


updateLinearScaledValue : Model -> ( Model, Cmd Msg )
updateLinearScaledValue model =
    case String.toFloat model.userLinearInput of
        Nothing ->
            ( { model | calculatedRangeValue = "Error" }, Cmd.none )

        Just value ->
            if
                value
                    <= model.selectedInputRange.max
                    && value
                    >= model.selectedInputRange.min
            then
                let
                    mdl =
                        { model | userLinearInput = String.fromFloat value }
                in
                ( { model | calculatedRangeValue = String.fromFloat (scaleLinear mdl) }, Cmd.none )

            else
                ( { model | calculatedRangeValue = "Out of range..." }, Cmd.none )


scaleLinear : Model -> Float
scaleLinear model =
    let
        ymax =
            model.selectedOutputRange.max

        ymin =
            model.selectedOutputRange.min

        xmax =
            model.selectedInputRange.max

        xmin =
            model.selectedInputRange.min

        inp : Float
        inp =
            case String.toFloat model.userLinearInput of
                Nothing ->
                    0

                Just val ->
                    val
    in
    (ymax - ymin) / (xmax - xmin) * (inp - xmin) + ymin


updateLogScaledValue : Model -> ( Model, Cmd Msg )
updateLogScaledValue model =
    let
        input =
            case String.toFloat model.userLogInput of
                Nothing ->
                    "Error..."

                Just val ->
                    String.fromFloat (scaleLogarithmic model.selectedPressureScale val)
    in
    ( { model | calculatedPressureValue = input }, Cmd.none )


scaleLogarithmic : PressureScale -> Float -> Float
scaleLogarithmic scale value =
    case scale of
        Torr ->
            10 ^ (value - 6)

        Millibar ->
            10 ^ (value - 6)

        Pascal ->
            10 ^ (value - 4)



-- VIEW


viewRangeOption : String -> RangeItem -> Html a
viewRangeOption defaultOption item =
    option
        [ selected (item.name == defaultOption) ]
        [ text item.name ]


viewDropdown : (String -> Msg) -> String -> String -> Html Msg
viewDropdown msg defaultOption lblText =
    tr [ class "ml2 v-mid" ]
        [ td [] [ text lblText ]
        , td []
            [ select
                [ onSelectedChange msg ]
                (List.map (viewRangeOption defaultOption) rangeItem)
            ]
        ]


viewRadioButtons : Model -> PressureScale -> Html Msg
viewRadioButtons model scale =
    label []
        [ input
            [ type_ "radio"
            , name "pressureScale"
            , checked (scale == model.selectedPressureScale)
            , onClick (SelectedScale scale)
            ]
            []
        , text (pressureScaleToString scale)
        ]


viewUserInput : String -> String -> (String -> Msg) -> Html Msg
viewUserInput inValue outValue msg =
    label []
        [ input
            [ placeholder "input..."
            , value inValue
            , onInput msg
            ]
            []
        , span [ class "bg-yellow black ph2 ma1" ]
            [ text ("Scaled value: " ++ outValue) ]
        ]


segmentFooter : String -> Html a
segmentFooter lblText =
    p [ class "f6" ] [ text lblText ]


pressureScaleToString : PressureScale -> String
pressureScaleToString item =
    case item of
        Torr ->
            "Torr"

        Millibar ->
            "Millibar"

        Pascal ->
            "Pascal"


viewCalculation : Html a
viewCalculation =
    p [ class "pa1 ma1 f6" ]
        [ span [] [ text "Calculation: (Y" ]
        , sub [] [ text "2" ]
        , span [] [ text " - " ]
        , span [] [ text "Y" ]
        , sub [] [ text "1" ]
        , span [] [ text ") / (X" ]
        , sub [] [ text "2" ]
        , span [] [ text " - " ]
        , span [] [ text "X" ]
        , sub [] [ text "1" ]
        , span [] [ text ") * (input - X" ]
        , sub [] [ text "1" ]
        , span [] [ text ") + Y" ]
        , sub [] [ text "1" ]
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ div [ class "sans-serif bg-mid-gray yellow ma1" ]
            [ h2 [ class "ml2" ] [ text "Linear Scaler" ]
            , table []
                [ viewDropdown OutputRangeSelected "800 to 2500 °C" "I want to know: "
                , viewDropdown InputRangeSelected "4 to 20 mA" "When I know: "
                ]
            , viewCalculation
            , div []
                [ hr
                    []
                    []
                , label [ class "ml2" ] [ text "Input: " ]
                , viewUserInput model.userLinearInput model.calculatedRangeValue UserLinearInputChange
                , segmentFooter "Linear Scaler (Calc2.elm)"
                ]
            ]
        , div [ class "sans-serif bg-mid-gray yellow ma1 fl w-100" ]
            [ h2 [ class "ml2" ] [ text "Logarithmic Scaler" ]
            , div [ class "flex flex-column ma1 pa1" ]
                (List.map (viewRadioButtons model) [ Torr, Millibar, Pascal ])
            , label [] [ text "Input Voltage:" ]
            , viewUserInput model.userLogInput model.calculatedPressureValue UserLogInputChange
            , segmentFooter "Logarithmic Scaler (Calc2.elm)"
            ]
        ]
