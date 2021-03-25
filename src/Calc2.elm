module Calc2 exposing (..)

import Browser
import Html exposing (Attribute, Html, a, div, hr, input, label, option, p, select, text)
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
            { model | userInput = value }

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
        , div [] [ text "(Calc2)" ]
        ]


rangeOption : RangeItem -> Html a
rangeOption item =
    option
        []
        [ text item.name ]



-- todo: work out how to make controls initialize to default selections
-- todo: styling
-- todo: labels are not clear on intent
