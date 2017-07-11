module Habits exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Dict
import Types exposing (..)
import Time.Date as TimeDate
import Util


viewDay date =
    let
        weekday =
            Util.viewWeekday date

        month =
            TimeDate.month date

        day =
            TimeDate.day date
    in
        String.join "" [ weekday, " ", toString month, "/", toString day ]


getDailyInfo : TimeDate.Date -> { previous : PeriodInfo, current : PeriodInfo }
getDailyInfo today =
    let
        yesterday =
            TimeDate.addDays -1 today
    in
        { current =
            { date = today
            , title = "today"
            , description = viewDay today
            , id = TimeDate.toISO8601 today
            }
        , previous =
            { date = yesterday
            , title = "yest."
            , description = viewDay yesterday
            , id = TimeDate.toISO8601 yesterday
            }
        }


getLastMonday date =
    let
        daysSinceLastMonday =
            case TimeDate.weekday date of
                TimeDate.Mon ->
                    0

                TimeDate.Tue ->
                    1

                TimeDate.Wed ->
                    2

                TimeDate.Thu ->
                    3

                TimeDate.Fri ->
                    4

                TimeDate.Sat ->
                    5

                TimeDate.Sun ->
                    6
    in
        TimeDate.addDays -daysSinceLastMonday date


getWeeklyInfo : TimeDate.Date -> { previous : PeriodInfo, current : PeriodInfo }
getWeeklyInfo today =
    let
        monday =
            getLastMonday today

        lastMonday =
            TimeDate.addDays -7 monday
    in
        { current =
            { date = monday
            , title = "this week"
            , description = String.join " " [ "week of", viewDay monday ]
            , id = TimeDate.toISO8601 monday
            }
        , previous =
            { date = lastMonday
            , title = "last week"
            , description = String.join " " [ "week of", viewDay lastMonday ]
            , id = TimeDate.toISO8601 lastMonday
            }
        }


viewMonth date =
    case TimeDate.month date of
        1 ->
            "Jan"

        2 ->
            "Feb"

        3 ->
            "Mar"

        4 ->
            "Apr"

        5 ->
            "May"

        6 ->
            "June"

        7 ->
            "July"

        8 ->
            "Aug"

        9 ->
            "Sep"

        10 ->
            "Oct"

        11 ->
            "Nov"

        12 ->
            "Dec"

        _ ->
            ""


getMonthlyInfo : TimeDate.Date -> { previous : PeriodInfo, current : PeriodInfo }
getMonthlyInfo today =
    let
        firstDay =
            TimeDate.setDay 1 today

        firstDayofPrevious =
            TimeDate.addMonths -1 firstDay
    in
        { current =
            { date = firstDay
            , title = viewMonth firstDay |> String.toLower
            , description = ""
            , id = TimeDate.toISO8601 firstDay
            }
        , previous =
            { date = firstDayofPrevious
            , title = viewMonth firstDayofPrevious |> String.toLower
            , description = ""
            , id = TimeDate.toISO8601 firstDayofPrevious
            }
        }


viewHabitTable getInfo today habits =
    let
        info =
            getInfo today

        header =
            tr []
                [ th [ title info.previous.description ] [ text info.previous.title ]
                , th [ title info.current.description ] [ text info.current.title ]
                , th [] []
                ]

        viewIcon complete =
            if complete then
                i [ class "fa fa-check" ] []
            else
                i [ class "fa fa-close" ] []

        viewHabit habit =
            let
                previousComplete =
                    Dict.get info.previous.id habit.periods
                        |> Maybe.withDefault False

                currentComplete =
                    Dict.get info.current.id habit.periods
                        |> Maybe.withDefault False
            in
                tr []
                    [ td
                        [ onClick (HabitsPage (TogglePeriod habit.id info.previous.id))
                        , classList
                            [ ( "habit-period", True )
                            , ( "complete", previousComplete )
                            , ( "incomplete", not previousComplete )
                            ]
                        ]
                        [ viewIcon previousComplete
                        ]
                    , td
                        [ onClick (HabitsPage (TogglePeriod habit.id info.current.id))
                        , classList
                            [ ( "habit-period", True )
                            , ( "complete", currentComplete )
                            , ( "incomplete", not currentComplete )
                            ]
                        ]
                        [ viewIcon currentComplete ]
                    , td []
                        [ input
                            [ placeholder "insert habit here"
                            , defaultValue habit.title
                            , onInput (\newTitle -> HabitsPage (SetHabitTitle habit.id newTitle))
                            , classList
                                [ ( "complete", currentComplete ) ]
                            ]
                            []
                        ]
                    ]
    in
        table
            []
            (header :: List.map viewHabit habits)


view model =
    let
        getHabits habitType =
            Dict.toList model.habits
                |> List.filter (\( ( thisHabitType, habitIndex ), habit ) -> thisHabitType == habitType)
                |> List.map (\( habitID, habit ) -> habit)

        maybeViewTable today ( habitType, getInfo, errorMessage ) =
            let
                habits =
                    getHabits habitType
            in
                if (List.length habits) == 5 then
                    viewHabitTable getInfo today habits
                else
                    div []
                        [ viewHabitTable getInfo today habits
                        , Util.viewError errorMessage
                        ]

        page today =
            div [ class "habits animated " ]
                [ h3 [] [ text "daily, weekly, & monthly habits" ]
                , maybeViewTable today ( "daily", getDailyInfo, "Weird. I couldn't find your daily habits." )
                , maybeViewTable today ( "weekly", getWeeklyInfo, "Weird. I couldn't find your weekly habits." )
                , maybeViewTable today ( "monthly", getMonthlyInfo, "Weird. I couldn't find your monthly habits." )
                ]
    in
        model.today
            |> Maybe.map page
            |> Maybe.withDefault (Util.viewError "Seems like there was a problem getting today's date.")
