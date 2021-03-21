module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, a, div, hr, input, label, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput)
import Json.Decode as Decode



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { yMax : String
    , yMin : String
    , xMax : String
    , xMin : String
    , userInput : String
    , selectedInputRange : String
    , selectedOutputRange : ( String, Float, Float )
    }


type alias RangeItem =
    { name : String
    , min : Float
    , max : Float
    }


init : Model
init =
    { yMax = "20"
    , yMin = "4"
    , xMax = "2500"
    , xMin = "800"
    , userInput = "4"
    , selectedInputRange = "..." --inputRangeItem [ 0 ]
    , selectedOutputRange = ( "...", 800, 2500 )
    }



-- inputRangeItem : List RangeItem
-- inputRangeItem =
--     [ { name = "0 to 20 mA", min = 0, max = 20 }
--     , { name = "4 to 20 mA", min = 4, max = 20 }
--     , { name = "-10 to 10 VDC", min = -10, max = 10 }
--     , { name = "0 to 10 VDC", min = 0, max = 10 }
--     ]


inputRangeItem : List String
inputRangeItem =
    [ "0 to 20 mA"
    , "4 to 20 mA"
    , "-10 to 10 VDC"
    , "0 to 10 VDC"
    ]



-- UPDATE


type Msg
    = YMaxChange String
    | YMinChange String
    | XMaxChange String
    | XMinChange String
    | UserInputChange String
    | InputRangeSelected String


update : Msg -> Model -> Model
update msg model =
    case msg of
        YMaxChange value ->
            { model | yMax = value }

        YMinChange value ->
            { model | yMin = value }

        XMaxChange value ->
            { model | xMax = value }

        XMinChange value ->
            { model | xMin = value }

        UserInputChange value ->
            { model | userInput = value }

        InputRangeSelected value ->
            { model | selectedInputRange = value }


onSelectedChange : (String -> msg) -> Attribute msg
onSelectedChange msg =
    on "change" (Decode.map msg Html.Events.targetValue)


scaleLinear : Model -> Float
scaleLinear model =
    let
        ymax : Float
        ymax =
            case String.toFloat model.yMax of
                Nothing ->
                    0

                Just val ->
                    val

        ymin : Float
        ymin =
            case String.toFloat model.yMin of
                Nothing ->
                    0

                Just val ->
                    val

        xmax : Float
        xmax =
            case String.toFloat model.xMax of
                Nothing ->
                    0

                Just val ->
                    val

        xmin : Float
        xmin =
            case String.toFloat model.xMin of
                Nothing ->
                    0

                Just val ->
                    val

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
    div []
        [ label [] [ text "yMax" ]
        , input [ placeholder "yMax", value model.yMax, onInput YMaxChange ] []
        , label [] [ text "yMin" ]
        , input [ placeholder "yMin", value model.yMin, onInput YMinChange ] []
        , hr [] []
        , label [] [ text "xMax" ]
        , input [ placeholder "xMax", value model.xMax, onInput XMaxChange ] []
        , label [] [ text "xMin" ]
        , input [ placeholder "xMin", value model.xMin, onInput XMinChange ] []
        , hr [] []
        , label [] [ text "userInput" ]
        , input [ placeholder "input", value model.userInput, onInput UserInputChange ] []
        , div [] [ text (String.fromFloat (scaleLinear model)) ]
        , hr [] []
        , label [] [ text "Input Range" ]
        , select [ onSelectedChange InputRangeSelected ] (List.map rangeOptions inputRangeItem)
        , div [] [ text model.selectedInputRange ]
        ]


rangeOptions : String -> Html a
rangeOptions item =
    option [] [ text item ]
