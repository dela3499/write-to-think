module Update exposing (..)

import Debug
import Dict
import String
import Types exposing (..)
import Date
import Time.Date as TimeDate
import Util
import Time
import Encode
import Ports


--maybe subupdates should operate on subset of model, rather than entire thing?


update msg model =
    case msg of
        OpenMenu ->
            ( { model | menuOpen = True }, Ports.setFocus "scribble-input" )

        CloseMenu ->
            ( { model | menuOpen = False }, Cmd.none )

        SetDate date ->
            setDate date model

        SetPage page ->
            ( { model | page = page }, Cmd.none )

        NotesPage notesPageMsg ->
            updateNotes notesPageMsg model

        ListsPage listsPageMsg ->
            updateLists listsPageMsg model

        HabitsPage habitsPageMsg ->
            updateHabits habitsPageMsg model

        PlansPage plansPageMsg ->
            updatePlans plansPageMsg model

        Tick time ->
            let
                diff =
                    model.timer - model.time

                diff2 =
                    if diff > 0 then
                        diff / 60000
                    else
                        0
            in
                ( { model | time = time }, Util.runDateTask )

        SaveModelLocally ->
            ( model, Ports.setLocalStorage (Encode.modelToJson model) )

        UrlUpdate pathname ->
            ( model, Cmd.none )

        SetTimer time ->
            ( { model | timer = time }, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )

        SetScribble scribble ->
            ( { model | scribble = scribble }, Cmd.none )

        Menu menuMsg ->
            updateMenu menuMsg model


updateMenu msg model =
    case msg of
        GoToNote i ->
            ( { model
                | activeNote = i
                , page = Notes
                , notesFullscreen = True
                , menuOpen = False
              }
            , Cmd.none
            )

        GoToListGroup i ->
            ( { model
                | listGroup = i
                , page = Lists
                , notesFullscreen = False
                , menuOpen = False
              }
            , Cmd.none
            )

        GoToPlans ->
            ( { model
                | page = Plans
                , notesFullscreen = False
                , menuOpen = False
              }
            , Cmd.none
            )

        GoToHabits ->
            ( { model
                | page = Habits
                , notesFullscreen = False
                , menuOpen = False
              }
            , Cmd.none
            )



--case Util.urlToPage pathname of
--    Ok page ->
--        ( { model | page = page }, Cmd.none )
--    Err error ->
--        ( model, Cmd.none )
-- dates update every second


updatePlans msg model =
    case msg of
        SetTask id task ->
            ( { model | tasks = Dict.insert id task model.tasks }
            , Util.setLocalStorage
            )

        SetActual hour value ->
            ( { model | actualSchedule = Dict.insert hour value model.actualSchedule }
            , Util.setLocalStorage
            )

        SetTemplate hour value ->
            ( { model | templateSchedule = Dict.insert hour value model.templateSchedule }
            , Util.setLocalStorage
            )


updateNotes msg model =
    let
        updateNote f =
            let
                newNotes =
                    Dict.update model.activeNote (Maybe.map f) model.notes
            in
                ( { model | notes = newNotes }, Util.setLocalStorage )
    in
        case msg of
            SetActiveNote id ->
                ( { model
                    | activeNote = id
                    , notesFullscreen = True
                  }
                , Cmd.none
                )

            SetTitle title ->
                updateNote (\note -> { note | title = title })

            SetContent content ->
                updateNote (\note -> { note | content = content })

            SetFullscreen ->
                ( { model | notesFullscreen = True }, Cmd.none )

            SetNormalScreen ->
                ( { model | notesFullscreen = False }, Cmd.none )


setDate date model =
    let
        year =
            Date.year date

        month =
            case Date.month date of
                Date.Jan ->
                    1

                Date.Feb ->
                    2

                Date.Mar ->
                    3

                Date.Apr ->
                    4

                Date.May ->
                    5

                Date.Jun ->
                    6

                Date.Jul ->
                    7

                Date.Aug ->
                    8

                Date.Sep ->
                    9

                Date.Oct ->
                    10

                Date.Nov ->
                    11

                Date.Dec ->
                    12

        day =
            Date.day date

        timeDate =
            TimeDate.date year month day
    in
        ( { model | today = Just timeDate }, Cmd.none )


updateLists msg model =
    case msg of
        SetListGroup groupIndex ->
            ( { model | listGroup = groupIndex }, Cmd.none )

        SetGroupTitle newTitle ->
            let
                oldGroups =
                    model.listGroups

                newGroups =
                    Dict.insert model.listGroup newTitle oldGroups
            in
                ( { model | listGroups = newGroups }, Util.setLocalStorage )

        SetListTitle listID newTitle ->
            let
                updateListTitle oldList =
                    let
                        newLists =
                            Dict.insert
                                listID
                                { oldList | title = newTitle }
                                model.lists
                    in
                        ( { model | lists = newLists }, Util.setLocalStorage )
            in
                Dict.get listID model.lists
                    |> Maybe.map updateListTitle
                    |> Maybe.withDefault ( model, Cmd.none )

        SetListItem1 listID newItem ->
            let
                updateListItem oldList =
                    let
                        newLists =
                            Dict.insert
                                listID
                                { oldList | item1 = newItem }
                                model.lists
                    in
                        ( { model | lists = newLists }, Util.setLocalStorage )
            in
                Dict.get listID model.lists
                    |> Maybe.map updateListItem
                    |> Maybe.withDefault ( model, Cmd.none )

        SetListItem2 listID newItem ->
            let
                updateListItem oldList =
                    let
                        newLists =
                            Dict.insert
                                listID
                                { oldList | item2 = newItem }
                                model.lists
                    in
                        ( { model | lists = newLists }, Util.setLocalStorage )
            in
                Dict.get listID model.lists
                    |> Maybe.map updateListItem
                    |> Maybe.withDefault ( model, Cmd.none )

        SetListItem3 listID newItem ->
            let
                updateListItem oldList =
                    let
                        newLists =
                            Dict.insert
                                listID
                                { oldList | item3 = newItem }
                                model.lists
                    in
                        ( { model | lists = newLists }, Util.setLocalStorage )
            in
                Dict.get listID model.lists
                    |> Maybe.map updateListItem
                    |> Maybe.withDefault ( model, Cmd.none )


updateHabits msg model =
    let
        updateHabit id f =
            let
                newHabits =
                    Dict.update id (Maybe.map f) model.habits
            in
                ( { model | habits = newHabits }, Util.setLocalStorage )
    in
        case msg of
            SetHabitTitle id newTitle ->
                updateHabit
                    id
                    (\habit -> { habit | title = newTitle })

            TogglePeriod habitID periodID ->
                let
                    toggle x =
                        case x of
                            Just value ->
                                Just (not value)

                            -- set True if period unset
                            Nothing ->
                                Just True

                    updatePeriod habit =
                        { habit | periods = Dict.update periodID toggle habit.periods }
                in
                    updateHabit habitID updatePeriod
