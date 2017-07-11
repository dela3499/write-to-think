module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Json
import String
import Task
import Dict


type Msg
    = SetInputValue1 String
    | SetInputValue2 String
    | Toggle


type Page
    = Page1
    | Page2


main =
    Html.program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initModel =
    { value1 = "one-default"
    , value2 = "two-default"
    , page = Page1
    }


view model =
    case model.page of
        Page1 ->
            Keyed.node "div"
                []
                [ ( "a", div [ onClick Toggle ] [ text "Toggle" ] )
                , ( "b"
                  , input
                        [ onInput SetInputValue1
                        , defaultValue model.value1
                        ]
                        []
                  )
                , ( "c", div [] [ text (toString model) ] )
                ]

        Page2 ->
            Keyed.node "div"
                []
                [ ( "d", div [ onClick Toggle ] [ text "Toggle" ] )
                , ( "e"
                  , input
                        [ onInput SetInputValue2
                        , defaultValue model.value2
                        ]
                        []
                  )
                , ( "f", div [] [ text (toString model) ] )
                ]


update msg model =
    case msg of
        SetInputValue1 inputValue ->
            ( { model | value1 = inputValue }, Cmd.none )

        SetInputValue2 inputValue ->
            ( { model | value2 = inputValue }, Cmd.none )

        Toggle ->
            case model.page of
                Page1 ->
                    ( { model | page = Page2 }, Cmd.none )

                Page2 ->
                    ( { model | page = Page1 }, Cmd.none )
