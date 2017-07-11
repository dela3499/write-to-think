module Encode exposing (..)

import Dict exposing (Dict)
import Json.Encode exposing (..)
import Time.Date as TimeDate
import Types exposing (..)


modelToJson model =
    encode 4 (encodeModel model)


encodeModel model =
    object
        [ --( "lastAccessed", encodeDate model.today )
          --, ( "activeNote", int model.activeNote ) -- this is view code, should be encoded in url, not saved model
          --, ( "notesFullscreen", bool model.notesFullscreen )
          --, ( "page", string (toString model.page) )
          --, ( "listgroup", int model.listGroup )
          ( "notes", encodeNotes model.notes )
        , ( "listGroups", encodeListGroups model.listGroups )
        , ( "lists", encodeLists model.lists )
        , ( "habits", encodeHabits model.habits )
        , ( "tasks", encodeTasks model.tasks )
        , ( "actualSchedule", encodeSchedule model.actualSchedule )
        , ( "templateSchedule", encodeSchedule model.templateSchedule )
        ]


encodeSchedule : Dict String String -> Value
encodeSchedule schedule =
    Dict.toList schedule
        |> List.map (\( hour, content ) -> ( hour, string content ))
        |> object


encodeTasks : Dict String String -> Value
encodeTasks tasks =
    let
        encodeTask ( dateString, task ) =
            ( dateString
            , string task
            )
    in
        Dict.toList tasks
            |> List.map encodeTask
            |> object


encodeHabits : Dict HabitID Habit -> Value
encodeHabits habits =
    let
        encodeHabit habit =
            object
                [ ( "id", encodeHabitID habit.id )
                , ( "title", string habit.title )
                , ( "periods", encodeHabitPeriods habit.periods )
                ]

        encodeHabitID ( habitType, habitIndex ) =
            object
                [ ( "habitType", string habitType )
                , ( "habitIndex", int habitIndex )
                ]

        encodeHabitPeriods periods =
            Dict.toList periods
                |> List.map formatPeriod
                |> object

        formatPeriod ( dateString, complete ) =
            ( dateString
            , bool complete
            )
    in
        Dict.values habits
            |> List.map encodeHabit
            |> list


encodeLists : Dict ( Int, Int ) MyList -> Value
encodeLists lists =
    let
        encodeID ( listGroupIndex, listIndex ) =
            object
                [ ( "listGroupIndex", int listGroupIndex )
                , ( "listIndex", int listIndex )
                ]

        encodeList myList =
            object
                [ ( "id", encodeID myList.id )
                , ( "title", string myList.title )
                , ( "item1", string myList.item1 )
                , ( "item2", string myList.item2 )
                , ( "item3", string myList.item3 )
                ]
    in
        Dict.values lists
            |> List.map encodeList
            |> list


encodeListGroups : Dict Int String -> Value
encodeListGroups listGroups =
    Dict.toList listGroups
        |> List.map (\( k, v ) -> ( toString k, string v ))
        |> object


encodeNotes : Dict Int Note -> Value
encodeNotes notes =
    let
        encodeNote note =
            object
                [ ( "id", int note.id )
                , ( "title", string note.title )
                , ( "content", string note.content )
                ]
    in
        Dict.values notes
            |> List.map encodeNote
            |> list


encodeDate date =
    case date of
        Nothing ->
            null

        Just day ->
            string (TimeDate.toISO8601 day)
