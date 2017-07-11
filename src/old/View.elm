module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Dict
import Types exposing (..)
import Time.Date as TimeDate
import Util
import Habits
import Encode


stylesheet path =
    Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href path ] []


timerTimes =
    [ 5, 15, 30, 60 ]


view model =
    case ( model.page, model.notesFullscreen ) of
        ( Notes, True ) ->
            viewFullscreenNotes model

        _ ->
            div
                []
                [ div [ class "top-section" ]
                    [ div [ class "container" ] [ viewHeader model ] ]
                , Keyed.node "div"
                    [ class "container bottom-section" ]
                    [ case model.page of
                        Notes ->
                            ( String.join " " [ "notes", toString model.activeNote ]
                            , viewNotes model
                            )

                        Lists ->
                            ( String.join " " [ "lists", toString model.listGroup ]
                            , viewLists model
                            )

                        Habits ->
                            ( "habits", Habits.view model )

                        Plans ->
                            ( "plans", viewPlans model )

                        _ ->
                            ( "empty", div [] [] )
                    ]

                --, viewFooter model
                , viewTimerWarning model
                , viewMenu model
                ]


pageLink label msg =
    h3
        [ class "page link pointer"
        , onClick msg
        ]
        [ text label ]


viewMenu model =
    if not model.menuOpen then
        div [] []
    else
        div []
            [ div
                [ class "menu-clickaway animated"
                , onClick CloseMenu
                ]
                []
            , div [ class "menu-wrapper absolute-centered" ]
                [ div [ class "menu animated" ]
                    [ div [ class "notes-and-lists" ]
                        [ viewNoteLinks model
                        , viewListLinks model
                        ]
                    , pageLink "plans" (Menu GoToPlans)
                    , pageLink "habits" (Menu GoToHabits)

                    --, viewPageLinks model
                    --, viewScribbleInput model
                    --, timer model
                    --, viewFeedbackInput model
                    ]
                ]
            ]


viewNoteLinks model =
    let
        viewNoteLink id =
            let
                maybeNote =
                    Dict.get id model.notes
            in
                case maybeNote of
                    Nothing ->
                        div [] []

                    Just note ->
                        div
                            [ onClick (Menu (GoToNote note.id))
                            , class "pointer"
                            ]
                            [ if note.title == "" then
                                text "untitled"
                              else
                                text note.title
                            ]

        links =
            List.range 0 4
                |> List.map viewNoteLink

        noteHeader =
            h3 [] [ text "notes" ]
    in
        div [ class "note links" ] (noteHeader :: links)


viewListLinks model =
    let
        viewListLink id =
            let
                maybeTitle =
                    Dict.get id model.listGroups
            in
                case maybeTitle of
                    Nothing ->
                        div [] []

                    Just title ->
                        div
                            [ onClick (Menu (GoToListGroup id))
                            , class "pointer"
                            ]
                            [ if title == "" then
                                text "untitled"
                              else
                                text title
                            ]

        links =
            List.range 0 4
                |> List.map viewListLink

        header =
            h3 [] [ text "lists" ]
    in
        div [ class "list links" ] (header :: links)


viewPageLinks model =
    div [] []


viewFeedbackInput model =
    div [] []


viewScribbleInput model =
    input
        [ class "scribble"
        , id "scribble-input"
        , placeholder "scribble away..."
        , onInput SetScribble
        , defaultValue model.scribble
        ]
        []


viewModelFooter model =
    div [ class "container" ]
        [ text (Encode.modelToJson model) ]


viewFooter model =
    case model.today of
        Nothing ->
            div [] []

        Just today ->
            let
                todayID =
                    TimeDate.toISO8601 today

                task =
                    Dict.get todayID model.tasks |> Maybe.withDefault ""
            in
                div
                    [ id "footer" ]
                    [ div [ class "container" ]
                        [ if task /= "" then
                            span [ class "mit-label" ] [ text "today's most important task: " ]
                          else
                            span
                                [ class "bold pointer"
                                , onClick (SetPage Plans)
                                ]
                                [ text "What's today's most important task?" ]
                        , span [] [ text task ]
                        ]
                    ]


viewHeader model =
    div [ class "header" ]
        [ div [ class "site-title" ] [ text "simple daily tools" ]
        , div [ class "nav" ]
            [ div [ class "nav-left" ]
                [ viewNavButton
                    { currentPage = model.page
                    , label = "notes"
                    , description = "Write a blog post, generate business ideas, review or plan your day."
                    , page = Notes
                    }
                , viewNavButton
                    { currentPage = model.page
                    , label = "lists"
                    , description = "Keep your business ideas, goals, errands, or reading list here."
                    , page = Lists
                    }
                , viewNavButton
                    { currentPage = model.page
                    , label = "habits"
                    , description = "Review and improve your habits."
                    , page = Habits
                    }
                , viewNavButton
                    { currentPage = model.page
                    , label = "plans"
                    , description = "Plan today, and your ideal day."
                    , page = Plans
                    }

                --, div [ class "nav-right" ]
                --    [ viewNavButton
                --        { currentPage = model.page
                --        , label = "settings"
                --        , description = ""
                --        , page = Settings
                --        }
                --    , span [] [ text "|" ]
                --    , viewNavButton
                --        { currentPage = model.page
                --        , label = "music"
                --        , description = ""
                --        , page = Settings
                --        }
                , span [ style [ ( "opacity", "0.7" ) ] ] [ text "|" ]
                , timer model
                ]
            ]
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


timer model =
    let
        diffTime =
            model.timer - model.time

        timerButton nMinutes =
            span
                [ class "timer-button pointer"
                , onClick (SetTimer (nMinutes * 60000 + model.time))

                --, onClick (SetTimer (3000 + model.time))
                ]
                [ text (toString nMinutes ++ "m") ]
    in
        span [ class "timer" ]
            [ if diffTime > 0 then
                span []
                    [ span [] [ text (formatMilliseconds diffTime) ]
                    , i
                        [ onClick (SetTimer 0)
                        , class "pointer close fa fa-close"
                        ]
                        []
                    ]
              else
                span [ class "timer-buttons" ]
                    (List.map timerButton timerTimes)
            ]


viewTimerWarning model =
    let
        diffTime =
            model.timer - model.time
    in
        if (diffTime < 1000) && (diffTime > -5000) then
            if diffTime > -1000 then
                div [ class "timer-warning-wrapper" ]
                    [ div [ class "timer-warning animated bounceIn" ] [ text "time expired" ] ]
            else
                div [ class "timer-warning-wrapper" ]
                    [ div [ class "timer-warning animated bounceOut" ] [ text "time expired" ] ]
        else
            div [] []


viewNavButton navModel =
    span
        [ classList
            [ ( "nav-button pointer", True )
            , ( "active", navModel.page == navModel.currentPage )
            ]
        , onClick (SetPage navModel.page)

        --, title (navModel.description)
        ]
        [ text navModel.label ]


viewNotes model =
    let
        viewNoteNav note =
            span
                [ classList
                    [ ( "nav-button pointer", True )
                    , ( "active", note.id == model.activeNote )
                    ]
                , onClick (NotesPage (SetActiveNote note.id))
                ]
                [ if note.title == "" then
                    text "untitled"
                  else
                    text note.title
                ]

        viewNotePreview note =
            div
                [ class "note-preview"
                , onClick (NotesPage (SetActiveNote note.id))
                ]
                [ h3 []
                    [ if note.title == "" then
                        text "untitled note"
                      else
                        text note.title
                    ]
                , p [] [ text note.content ]
                ]

        fullscreenButton =
            span
                [ class "nav-button pointer float-right"
                , onClick (NotesPage SetFullscreen)
                ]
                [ text "fullscreen"
                ]

        noteTitle =
            Dict.get model.activeNote model.notes
                |> Maybe.map viewTitle
                |> Maybe.withDefault (div [] [])
    in
        div
            [ class "notes animated "
            ]
            [ div [ class "listgroup-nav" ]
                (model.notes
                    |> Dict.values
                    |> List.map viewNotePreview
                 --|> List.map viewNoteNav
                 --|> List.intersperse (span [ class "subtle" ] [ text "/" ])
                 --|> List.append [ fullscreenButton ]
                )

            --, noteTitle
            --, Dict.get model.activeNote model.notes
            --    |> Maybe.map viewNote
            --    |> Maybe.withDefault (Util.viewError "I can't find that note for some reason.")
            ]


viewTitle note =
    div
        [ class "note title" ]
        [ input
            [ placeholder "enter title here"
            , defaultValue note.title
            , onInput (\newTitle -> NotesPage (SetTitle newTitle))
            ]
            []
        ]


viewNote note =
    let
        computeOpacity i =
            clamp 0 1 ((1 - (toFloat i / 5000)) + 0.8)

        viewChar i char =
            char
                |> String.fromChar
                |> (\char ->
                        if char == "\n" then
                            br [] []
                        else
                            span [ class "dummy-char", style [ ( "opacity", toString (computeOpacity i) ) ] ] [ text char ]
                   )

        chars =
            note.content
                |> String.toList
                |> List.indexedMap viewChar
    in
        div
            [ class "text-wrapper" ]
            [ textarea
                [ placeholder "type here ..."
                , onInput (\content -> (NotesPage (SetContent content)))
                , defaultValue note.content
                ]
                []

            --, div [ class "dummy-textarea" ]
            --chars
            ]


viewFullscreenNotes model =
    let
        normalScreenButton =
            span
                [ class " pointer leave-fullscreen"
                , onClick (NotesPage SetNormalScreen)
                ]
                [ text "back to notes"
                ]

        maybeNote =
            Dict.get model.activeNote model.notes
    in
        case maybeNote of
            Nothing ->
                Util.viewError "I couldn't find that note."

            Just note ->
                div []
                    [ Keyed.node "div"
                        [ class "fullscreen-notes animated " ]
                        [ ( "notes-header"
                          , div [ class "fullscreen-header" ]
                                [ timer model
                                , span [ class "divider" ] [ text "|" ]
                                , normalScreenButton
                                ]
                          )
                        , ( (toString note.id) ++ "-title", viewTitle note )
                        , ( (toString note.id) ++ "-note", Lazy.lazy viewNote note )
                        ]
                    , div [ class "dark-background" ] []
                    , viewTimerWarning model
                    , viewMenu model
                    ]


viewLists model =
    let
        viewListGroupNav ( groupIndex, groupTitle ) =
            span
                [ classList
                    [ ( "listgroup nav-button pointer", True )
                    , ( "active", groupIndex == model.listGroup )
                    ]
                , onClick (ListsPage (SetListGroup groupIndex))
                ]
                [ if groupTitle == "" then
                    text "untitled"
                  else
                    text groupTitle
                ]

        viewList listIndex =
            let
                listKey =
                    ( model.listGroup, listIndex )

                maybeList =
                    Dict.get listKey model.lists
            in
                case maybeList of
                    Just list ->
                        div [ class "list" ]
                            [ input
                                [ class "title"
                                , placeholder "_________"
                                , onInput (\newTitle -> ListsPage (SetListTitle listKey newTitle))
                                , defaultValue list.title
                                ]
                                []
                            , input
                                [ class "item"
                                , placeholder "_________"
                                , onInput (\newItem -> ListsPage (SetListItem1 listKey newItem))
                                , defaultValue list.item1
                                ]
                                []
                            , input
                                [ class "item"
                                , placeholder "_________"
                                , onInput (\newItem -> ListsPage (SetListItem2 listKey newItem))
                                , defaultValue list.item2
                                ]
                                []
                            , input
                                [ class "item"
                                , placeholder "_________"
                                , onInput (\newItem -> ListsPage (SetListItem3 listKey newItem))
                                , defaultValue list.item3
                                ]
                                []
                            ]

                    Nothing ->
                        div [] []

        groupTitle =
            let
                label =
                    Dict.get model.listGroup model.listGroups

                viewLabel title =
                    div
                        [ class "listgroup title" ]
                        [ input
                            [ placeholder "enter title here"
                            , defaultValue title
                            , onInput (\newTitle -> ListsPage (SetGroupTitle newTitle))
                            ]
                            []
                        ]
            in
                label
                    |> Maybe.map viewLabel
                    |> Maybe.withDefault (div [] [ text "no title" ])
    in
        div [ class "lists animated " ]
            [ div [ class "listgroup-nav" ]
                (Dict.toList model.listGroups
                    |> List.map viewListGroupNav
                    |> List.intersperse (span [ class "subtle" ] [ text "/" ])
                )
            , groupTitle
            , div [ class "column col1" ]
                (List.map viewList [ 0, 1, 2 ])
            , div [ class "column" ]
                (List.map viewList [ 3, 4, 5 ])
            ]


viewMITS model =
    case model.today of
        Nothing ->
            Util.viewError "There seems to be a problem getting today's date."

        Just today ->
            let
                days =
                    List.range -1 5
                        |> Util.zipWith (\offset -> TimeDate.addDays offset today)

                viewDay ( diff, date ) =
                    let
                        id =
                            TimeDate.toISO8601 date

                        task =
                            Dict.get id model.tasks
                                |> Maybe.withDefault ""

                        displayName =
                            if diff == 0 then
                                "today"
                                --String.join "" [ "(", "Today", ") ", Util.viewWeekday date ]
                            else
                                String.toLower (Util.viewWeekday date)
                    in
                        tr
                            [ classList
                                [ ( "task", True )
                                , ( "active", date == today )
                                ]
                            ]
                            [ td
                                []
                                [ text displayName
                                ]
                            , td []
                                [ input
                                    [ defaultValue task
                                    , placeholder "__________________"
                                    , onInput (\newTask -> PlansPage (SetTask id newTask))
                                    ]
                                    []
                                ]
                            ]
            in
                table [] (List.map viewDay days)


viewPlans model =
    let
        hours =
            List.range 0 23
                |> List.map (\t -> Util.formatHour (t + 5))

        viewActualHour hour =
            let
                label =
                    Dict.get hour model.actualSchedule |> Maybe.withDefault ""
            in
                tr []
                    [ td [ class "hour" ] [ text hour ]
                    , td []
                        [ input
                            [ defaultValue label
                            , placeholder "__________________"
                            , onInput (\value -> PlansPage (SetActual hour value))
                            ]
                            []
                        ]
                    ]

        viewTemplateHour hour =
            let
                label =
                    Dict.get hour model.templateSchedule |> Maybe.withDefault ""
            in
                tr []
                    [ td [ class "hour" ] [ text hour ]
                    , td []
                        [ input
                            [ defaultValue label
                            , placeholder "__________________"
                            , onInput (\value -> PlansPage (SetTemplate hour value))
                            ]
                            []
                        ]
                    ]

        actualSchedule =
            div [ class "actual" ]
                [ h3 [] [ text "today's schedule" ]
                , table [] (List.map viewActualHour hours)
                ]

        templateSchedule =
            div [ class "template" ]
                [ h3 [] [ text "template" ]
                , table [] (List.map viewTemplateHour hours)
                ]
    in
        div
            [ class "plans animated " ]
            [ div [ class "tasks" ]
                [ h3 [] [ text "most important tasks" ]
                , viewMITS model
                ]
            , div [ class "schedules" ]
                [ actualSchedule
                , templateSchedule
                ]
            ]
