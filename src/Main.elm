module Main exposing (main)

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Input as Input


type alias Model =
    { input: Input
    }


type Input
    = NumberInput Int
    | PartialInput String
    | EmptyInput


type Msg
    = ChangeInput Input
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
        ChangeInput input -> { model | input = input }

        BadInput -> model


view : Model -> Html Msg
view model = 
    Element.layout [] (viewPage model)


viewPage : Model -> Element Msg
viewPage model = 
    column []
        [ Element.text "Hello World"
        , Input.text []
            { text = case model.input of
                EmptyInput -> ""
                PartialInput str -> str
                NumberInput n -> String.fromInt n
            , onChange = \s ->
                    case s of
                        "" -> ChangeInput EmptyInput
                        "-" -> ChangeInput <| PartialInput s
                        _ ->
                            case String.toInt s of
                                Nothing -> BadInput
                                Just n -> ChangeInput <| NumberInput n
            , label = Input.labelLeft [] (text "Enter a base-10 number")
            , placeholder = Nothing
            }
        , row []
            (case model.input of
                NumberInput n -> [Element.text (stringFromIntInBase 6 n)]
                _ -> [])
        ]

