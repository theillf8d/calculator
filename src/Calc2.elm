module Calc2 exposing (..)

import Browser
import Html exposing (Attribute, Html, a, div, h2, hr, input, label, option, p, select, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput)
import Json.Decode as D



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { userInput : String
    , selectedInputRange : RangeItem
    , selectedOutputRange : RangeItem
    , calcualtedValue : String
    , firstPass : Bool
    }


type alias RangeItem =
    { name : String
    , min : Float
    , max : Float
    }


rangeItem : List RangeItem
rangeItem =
    [ { name = "0 to 20 mA", min = 0, max = 20 }
    , { name = "4 to 20 mA", min = 4, max = 20 }
    , { name = "-10 to 10 VDC", min = -10, max = 10 }
    , { name = "0 to 10 VDC", min = 0, max = 10 }
    , { name = "800 to 2500 °C", min = 800, max = 2500 }
    , { name = "1000 to 2500 °C", min = 1000, max = 2500 }
    , { name = "0 to 5000 °C", min = 0, max = 5000 }
    , { name = "1000 to 5000 °C", min = 1000, max = 5000 }
    ]


init : Model
init =
    { userInput = "4"
    , selectedInputRange = { name = "4 to 20mA", min = 4, max = 20 }
    , selectedOutputRange = { name = "800 to 2500 °C", min = 800, max = 2500 }
    , calcualtedValue = "4"
    , firstPass = True
    }



-- UPDATE


type Msg
    = UserInputChange String
    | InputRangeSelected String
    | OutputRangeSelected String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserInputChange value ->
            let
                newVal =
                    case String.toFloat value of
                        Nothing ->
                            "Error"

                        Just val ->
                            if
                                val
                                    <= model.selectedInputRange.max
                                    && val
                                    >= model.selectedInputRange.min
                            then
                                let
                                    mdl =
                                        { model | userInput = String.fromFloat val }
                                in
                                String.fromFloat (scaleLinear mdl)

                            else
                                "Out of range..."
            in
            { model | userInput = value, calcualtedValue = newVal, firstPass = False }

        InputRangeSelected value ->
            { model | selectedInputRange = getSelectedRangeItem value }

        OutputRangeSelected value ->
            { model | selectedOutputRange = getSelectedRangeItem value }


getSelectedRangeItem : String -> RangeItem
getSelectedRangeItem value =
    case List.filter (\val -> val.name == value) rangeItem |> List.head of
        Nothing ->
            -- need to look at something different here
            { name = "0 to 20mA", min = 0, max = 20 }

        Just val ->
            val


onSelectedChange : (String -> msg) -> Attribute msg
onSelectedChange msg =
    on "change" (D.map msg Html.Events.targetValue)


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
    div
        [ class "sans-serif measure bg-mid-gray yellow ma1"
        ]
        [ h2 [ class "ml2" ] [ text "Linear Scaler" ]
        , p [ class "ml2" ]
            [ label [] [ text "I want to know: " ]
            , select
                [ onSelectedChange OutputRangeSelected ]
                (List.map outputRangeOption rangeItem)
            ]
        , p [ class "ml2" ]
            [ label [] [ text "When I know: " ]
            , select
                [ onSelectedChange InputRangeSelected ]
                (List.map inputRangeOption rangeItem)
            ]
        , hr
            []
            []
        , label [ class "ml2" ] [ text "Input: " ]
        , input
            [ placeholder "input"
            , value model.userInput
            , onInput UserInputChange
            ]
            []
        , span
            [ class "bg-yellow black ph2 ma1" ]
            [ text ("Scaled value: " ++ model.calcualtedValue) ]
        , p [ class "f6" ] [ text "(Calc2.elm)" ]
        ]


rangeOption : RangeItem -> String -> Html a
rangeOption item defaultItem =
    let
        _ =
            Debug.log "item: " item
    in
    if item.name == defaultItem then
        option
            [ selected True ]
            [ text item.name ]

    else
        option
            []
            [ text item.name ]


inputRangeOption : RangeItem -> Html a
inputRangeOption item =
    rangeOption item "4 to 20 mA"


outputRangeOption : RangeItem -> Html a
outputRangeOption item =
    rangeOption item "800 to 2500 °C"
