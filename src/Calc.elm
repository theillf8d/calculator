module Calc exposing (..)

import Browser
import Html exposing (Attribute, Html, a, div, hr, input, label, option, p, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput)
import Json.Decode as Decode



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { userInput : String
    , selectedInputRange : String
    , selectedOutputRange : String
    , xMax : Float
    , xMin : Float
    , yMax : Float
    , yMin : Float
    , input : Float
    }


type alias RangeItem =
    { name : String
    , min : Float
    , max : Float
    }


rangeItem : List String
rangeItem =
    [ "0 to 20 mA"
    , "4 to 20 mA"
    , "-10 to 10 VDC"
    , "0 to 10 VDC"
    , "800 to 2500 °C"
    , "1000 to 2500 °C"
    , "0 to 5000 °C"
    , "1000 to 5000 °C"
    ]


init : Model
init =
    { userInput = "4"
    , selectedInputRange = "4 to 20mA" --{ name = "0 to 20 mA", min = 0, max = 20 } --List.head inputRangeItem
    , selectedOutputRange = "0 to 5000 °C" --{ name = "0 to 5000 °C", min = 0, max = 20 } --List.head outputRangeItem
    , xMax = 5000
    , xMin = 0
    , yMax = 20
    , yMin = 4
    , input = 4
    }


type Msg
    = UserInputChange String
    | InputRangeSelected String
    | OutputRangeSelected String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserInputChange value ->
            { model | userInput = value }

        InputRangeSelected value ->
            case value of
                "0 to 20 mA" ->
                    { model | selectedInputRange = value, xMax = 20, xMin = 0 }

                "4 to 20 mA" ->
                    { model | selectedInputRange = value, xMax = 20, xMin = 4 }

                "-10 to 10 VDC" ->
                    { model | selectedInputRange = value, xMax = 10, xMin = -10 }

                "0 to 10 VDC" ->
                    { model | selectedInputRange = value, xMax = 10, xMin = 0 }

                "800 to 2500 °C" ->
                    { model | selectedInputRange = value, xMax = 2500, xMin = 0 }

                "1000 to 2500 °C" ->
                    { model | selectedInputRange = value, xMax = 2500, xMin = 0 }

                "0 to 5000 °C" ->
                    { model | selectedInputRange = value, xMax = 5000, xMin = 0 }

                "1000 to 5000 °C" ->
                    { model | selectedInputRange = value, xMax = 5000, xMin = 1000 }

                _ ->
                    { model | selectedInputRange = value, xMax = 20, xMin = 4 }

        OutputRangeSelected value ->
            case value of
                "0 to 20 mA" ->
                    { model | selectedOutputRange = value, yMax = 20, yMin = 0 }

                "4 to 20 mA" ->
                    { model | selectedOutputRange = value, yMax = 20, yMin = 4 }

                "-10 to 10 VDC" ->
                    { model | selectedOutputRange = value, yMax = 10, yMin = -10 }

                "0 to 10 VDC" ->
                    { model | selectedOutputRange = value, yMax = 10, yMin = 0 }

                "800 to 2500 °C" ->
                    { model | selectedOutputRange = value, yMax = 2500, yMin = 0 }

                "1000 to 2500 °C" ->
                    { model | selectedOutputRange = value, yMax = 2500, yMin = 0 }

                "0 to 5000 °C" ->
                    { model | selectedOutputRange = value, yMax = 5000, yMin = 0 }

                "1000 to 5000 °C" ->
                    { model | selectedOutputRange = value, yMax = 5000, yMin = 1000 }

                _ ->
                    { model | selectedOutputRange = value, yMax = 5000, yMin = 0 }


onSelectedChange : (String -> msg) -> Attribute msg
onSelectedChange msg =
    on "change" (Decode.map msg Html.Events.targetValue)


scaleLinear : Model -> Float
scaleLinear model =
    let
        ymax =
            model.yMax

        ymin =
            model.yMin

        xmax =
            model.xMax

        xmin =
            model.xMin

        inp : Float
        inp =
            case String.toFloat model.userInput of
                Nothing ->
                    0

                Just val ->
                    val
    in
    (ymax - ymin) / (xmax - xmin) * (inp - xmin) + ymin



-- VIEW


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "model: " model
    in
    div []
        [ p []
            [ label [] [ text "Input Range" ]
            , select [ onSelectedChange InputRangeSelected ] (List.map rangeOption rangeItem)
            ]
        , p []
            [ label [] [ text "Output Range" ]
            , select [ onSelectedChange OutputRangeSelected ] (List.map rangeOption rangeItem)
            ]
        , hr [] []
        , label [] [ text "Value to scale: " ]
        , input [ placeholder "input", value model.userInput, onInput UserInputChange ] []
        , div [] [ text ("Scaled value: " ++ String.fromFloat (scaleLinear model)) ]
        ]


rangeOption : String -> Html a
rangeOption item =
    option [] [ text item ]
