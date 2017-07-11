module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Task
import String
import Time exposing (Time)
import Array
import Debug
import Html.Keyed as Keyed


main =
    Html.program
        { init = ( initModel, Task.perform Tick Time.now )
        , view = view
        , update = update
        , subscriptions = \x -> Time.every (Time.second / 4) Tick
        }


initModel =
    { dark = True
    , promptState = AllTitles
    , text = ""
    , time = Nothing
    , alarm = Nothing
    }


type Msg
    = ToggleTheme
    | SetPrompt PromptState
    | SetText String
    | SetAlarm (Maybe Time)
    | Tick Time
    | NoMsg


type PromptState
    = Prompt Int
    | AllTitles
    | Maximized


update msg model =
    case msg of
        ToggleTheme ->
            ( { model | dark = not model.dark }, Cmd.none )

        SetPrompt promptState ->
            ( { model | promptState = promptState }, Cmd.none )

        SetText string ->
            ( { model | text = string }, Cmd.none )

        SetAlarm alarm ->
            ( { model | alarm = alarm }, Cmd.none )

        Tick time ->
            ( { model | time = Just time }, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )


view model =
    div
        [ id "app", classList [ ( "dark", model.dark ) ] ]
        [ viewHeader model
        , prompts model
        , if model.promptState /= Maximized then
            viewInput model
          else
            div [ id "input" ] []
        , timerPopup model
        ]


container x =
    div [ class "container" ] [ x ]


viewHeader model =
    let
        toggleText =
            if model.dark then
                "light theme"
            else
                "dark theme"

        themeToggle =
            span [ class "item", onClick ToggleTheme ] [ text toggleText ]

        viewTimer nMinutes =
            let
                msg =
                    case model.time of
                        Just time ->
                            (SetAlarm (Just (time + 60000 * nMinutes)))

                        Nothing ->
                            NoMsg
            in
                span
                    [ class "time-button item"
                    , onClick msg
                    ]
                    [ text (toString nMinutes) ]

        timerSection =
            case ( model.time, model.alarm ) of
                ( Nothing, Nothing ) ->
                    div [] []

                ( Just time, Nothing ) ->
                    span
                        [ class "timer-section" ]
                        [ viewTimer 5
                        , viewTimer 15
                        , viewTimer 30
                        ]

                ( Just time, Just alarm ) ->
                    span
                        [ class "timer-section item" ]
                        [ (alarm - time)
                            |> Basics.max 0
                            |> formatMilliseconds
                            |> text
                        , i
                            [ class "pointer close fa fa-close"
                            , onClick (SetAlarm Nothing)
                            ]
                            []
                        ]

                _ ->
                    div [] []
    in
        div
            [ id "header" ]
            [ div
                [ class "container" ]
                [ div [ class "site-title" ] [ text "write to think" ]
                , div [ class "float-right" ]
                    [ timerSection
                    , themeToggle
                    ]
                ]
            ]


prompts model =
    let
        viewPromptTitle i ( promptTitle, description ) =
            div
                [ class "prompt"
                , title description
                , onClick (SetPrompt (Prompt i))
                ]
                [ text promptTitle ]

        backButton =
            span
                [ class "back-button"
                , onClick (SetPrompt AllTitles)
                ]
                [ i [ class "fa fa-chevron-left" ] []
                ]

        viewPrompt index =
            let
                ( promptTitle, description ) =
                    promptInfo
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.withDefault ( "", "" )
            in
                [ backButton
                , span
                    [ class "prompt-description" ]
                    [ text description ]
                ]

        expandButton =
            div
                [ class "prompt expand-button"
                , onClick (SetPrompt Maximized)
                ]
                [ text "maximize" ]

        allTitles =
            (List.indexedMap viewPromptTitle promptInfo ++ [ expandButton ])

        viewPromptRow i ( promptTitle, description ) =
            tr
                [ onClick (SetPrompt (Prompt i)) ]
                [ td [] [ text promptTitle ]
                , td [] [ text description ]
                ]

        maximized =
            [ backButton
            , table
                []
                (List.indexedMap viewPromptRow promptInfo)
            ]

        content =
            case model.promptState of
                AllTitles ->
                    allTitles

                Prompt i ->
                    viewPrompt i

                Maximized ->
                    maximized
    in
        Keyed.node "div"
            [ id "prompts" ]
            [ ( toString model.promptState
              , div
                    [ class "container animated fadeIn" ]
                    content
              )
            ]


viewInput model =
    div
        [ id "input" ]
        [ div [ class "container" ]
            [ textarea
                [ placeholder "start thinking..."
                , defaultValue model.text
                , onInput SetText
                , maxlength 5000
                ]
                []
            , charCount model
            ]
        ]


timerPopup model =
    case ( model.time, model.alarm ) of
        ( Just time, Just alarm ) ->
            if time > alarm then
                div
                    [ class "popup" ]
                    [ div [ class "popup-text" ] [ text "time's up" ]
                    , div
                        [ class "pointer popup-close-button"
                        , onClick (SetAlarm Nothing)
                        ]
                        [ text "close" ]
                    ]
            else
                div [] []

        _ ->
            div [] []


charCount model =
    let
        nRemaining =
            5000 - (String.length model.text)
    in
        div
            [ class "char-count" ]
            [ text (toString nRemaining) ]


promptInfo =
    [ ( "3-things", "What are three things you can say about it?" )
    , ( "5w+h", "Who, what, when, where, why, how?" )
    , ( "80/20", "What 20% of your effort is producing 80% of your results?" )
    , ( "easy", "What would this look like if it was easy?" )
    , ( "absurd", "What are some absurd thoughts on this?" )
    , ( "future", "How will this work in a thousand years?" )
    , ( "stream", "Write the words that come to mind. (house, bird, whatever...)" )
    , ( "pscd", "State problem. Generate solutions. Compare them. Decide on action." )
    , ( "dialogue", "Write a dialog between yourself and someone you respect." )
    , ( "upshot", "What's the upshot/conclusion/takeaway/result of your analysis?" )
    , ( "opposite", "What if you did the opposite?" )
    , ( "money", "How could you throw money at the problem?" )
    , ( "timebox", "What if you could only spend an hour on this?" )
    , ( "long-term", "How can you maximize the chance of success in 5 years?" )
    , ( "subtract", "What if you could only remove things to solve the problem?" )
    , ( "matters", "Does this actually matter? Are there more important problems you should focus on?" )
    , ( "next", "What are your next steps?" )
    , ( "decisions", "What are the decisions you need to make?" )
    , ( "interview", "Pretend you're interviewing yourself." )
    ]


formatMilliseconds milliseconds =
    let
        totalSeconds =
            milliseconds / 1000

        minutes =
            floor (totalSeconds / 60)

        seconds =
            floor (totalSeconds - (60 * (toFloat minutes)))

        minuteString =
            toString minutes

        secondString =
            toString seconds
                |> String.padLeft 2 '0'
    in
        minuteString ++ ":" ++ secondString
