module Main exposing (main)

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Input as Input
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border


type alias Model =
    { input: Input
    }


type Input
    = NumberInput Int
    | PartialInput String
    | EmptyInput


type Msg
    = SetInput Input
    | Increase
    | Decrease
    | BadInput


stringFromIntInBase : Int -> Int -> String
stringFromIntInBase base number =
    if number < 0 then "-" ++ stringFromIntInBase base (-number) else
    if number == 0 then "0" else
        let last = number |> modBy base
            rest = number // base
            restStr = if rest > 0 then stringFromIntInBase base rest else ""
        in restStr ++ String.fromInt last


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    { input = EmptyInput
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetInput input -> { model | input = input }

        Increase ->
            case model.input of
                NumberInput number -> { model | input = NumberInput (number + 1) }
                _ -> model

        Decrease ->
            case model.input of
                NumberInput number -> { model | input = NumberInput (number - 1) }
                _ -> model

        BadInput -> model


view : Model -> Html Msg
view model = 
    Element.layout
        [ Background.color colors.dark ]
        (viewPage model)


viewPage : Model -> Element Msg
viewPage model = 
    column
        [ centerX ]
        [ viewHeading model
        , row [ width fill ] [ viewInput model, viewButtons model ]
        , viewOutput model
        ]


viewHeading : Model -> Element Msg
viewHeading model =
    el
        [ centerX
        , padding 20
        , Font.size 50
        , Font.family [ Font.monospace ]
        , Font.color colors.light
        ]
        (text "Seximal Converter")


viewInput : Model -> Element Msg
viewInput model =
    Input.text [ width (fillPortion 2) ]
        { text = case model.input of
            EmptyInput -> ""
            PartialInput str -> str
            NumberInput n -> String.fromInt n
        , onChange = \s ->
                case s of
                    "" -> SetInput EmptyInput
                    "-" -> SetInput <| PartialInput s
                    _ ->
                        case String.toInt s of
                            Nothing -> BadInput
                            Just n -> SetInput <| NumberInput n
        , label =
            Input.labelAbove
                [ Font.color colors.light
                , Font.family [ Font.monospace ]
                ]
                (text "enter a base-10 number")
        , placeholder = Nothing
        }


viewButtons : Model -> Element Msg
viewButtons model =
    let buttonStyle =
            [ height (px 40)
            , width (px 40)
            , spacing 105
            , Border.width 2
            , Border.color colors.light
            , Border.rounded 5
            ]
        label str = 
            el
                [ centerX, centerY
                , Font.family [ Font.monospace ]
                , Font.color colors.light
                ]
                (text str)
    in row
        [ width (fillPortion 1)
        , height fill
        ]
        [ Input.button buttonStyle
            { label = label "+"
            , onPress = Just Increase
            }
        , Input.button buttonStyle
            { label = label "-"
            , onPress = Just Decrease
            }
        ]


viewOutput : Model -> Element Msg
viewOutput model = 
    row
        [ centerX
        , padding 50
        , Font.color colors.saturated
        , Font.size 40
        , Font.extraBold
        ]
        (case model.input of
            NumberInput n -> [Element.text (stringFromIntInBase 6 n)]
            _ -> [])


colors : { dark: Color, light: Color, saturated: Color }
colors = 
    { dark = rgb255 10 11 10
    , light = rgb255 209 224 213
    , saturated = rgb255 24 181 66
    }

