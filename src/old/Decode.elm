module Decode exposing (..)

import Util
import Debug
import Json.Decode exposing (..)
import Types exposing (..)
import Dict


jsonToStoredModel json =
    let
        decodedModel =
            decodeString rawStoredModel json

        storedModel =
            Result.map getStoredModel decodedModel

        --_ =
        --    Debug.log "decoder" storedModel
    in
        storedModel


isOk x =
    case x of
        Ok _ ->
            True

        _ ->
            False


indexByID xs =
    xs
        |> List.map (\x -> ( x.id, x ))
        |> Dict.fromList


getStoredModel model =
    { notes = indexByID model.notes
    , listGroups =
        model.listGroups
            |> List.map (\( listGroupIndex, title ) -> ( String.toInt listGroupIndex, title ))
            |> List.filter (\( key, _ ) -> isOk key)
            -- all values should be 'ok' !
            |> List.map (\( key, value ) -> ( Result.withDefault 0 key, value ))
            |> Dict.fromList
    , lists = indexByID model.lists
    , habits = indexByID model.habits
    , tasks = model.tasks
    , actualSchedule = model.actualSchedule
    , templateSchedule = model.templateSchedule
    }


rawStoredModel =
    map7 RawStoredModel
        (field "notes" notes)
        (field "listGroups" listGroups)
        (field "lists" lists)
        (field "habits" habits)
        (field "tasks" tasks)
        (field "actualSchedule" schedule)
        (field "templateSchedule" schedule)


schedule =
    dict string


tasks =
    dict string


habits =
    list habit


habit =
    map3 Habit
        (field "id" habitID)
        (field "title" string)
        (field "periods" periods)


habitID =
    map2 (\a b -> ( a, b ))
        (field "habitType" string)
        (field "habitIndex" int)


periods =
    dict bool


lists =
    list myList


myList =
    map5 MyList
        (field "id" address)
        (field "title" string)
        (field "item1" string)
        (field "item2" string)
        (field "item3" string)


address =
    map2 (\k v -> ( k, v ))
        (field "listGroupIndex" int)
        (field "listIndex" int)


listGroups =
    (keyValuePairs string)


notes =
    list note


note =
    map3 Note
        (field "id" int)
        (field "title" string)
        (field "content" string)



--decodeModel json =
--    let
--        decode myField decoder = decodeString (field myField decoder) json
--    in
--        {notes = decode "notes" notes}
--model json =
--    let
--        json =
--            "{\"a\": {\"a\":10, \"b\":20}}"
--    in
--        decodeString (field "a" a) json
--type alias R =
--    { a : Int
--    , b : Int
--    }
--a =
--    map2 R (field "a" int) (field "b" int)
--notes =
--    list note
--note =
