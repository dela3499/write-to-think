module Util exposing (..)

import Html exposing (text, a, div)
import Html.Attributes exposing (href, class)
import Types exposing (..)
import Time.Date as TimeDate
import Task
import Date
import Ports
import Encode
import Dict
import UrlParser exposing (int, string, map, oneOf, s, parsePath, (</>))
import Time


--port module Ports exposing (..)
--port urlUpdate : (String -> msg) -> Sub msg
--port changeUrl : String -> Cmd msg
--port clearField : String -> Cmd msg
--pageToUrl page =
--  case page of
--    HomePage -> "/"
--    DownloadsPage -> "/downloads"
--    AboutPage -> "/about"
--    TopLevel PublicationsPage -> "/publications"
--    TopLevel CategoriesPage -> "/biomarker-types"
--    TopLevel BiomarkersPage -> "/biomarkers"
--    TopLevel ObservationsPage -> "/results"
--    BottomLevel (PublicationPage int) -> "/publications/" ++ (toString int)
--    BottomLevel (CategoryPage string) -> "/biomarker-types/" ++ string
--    BottomLevel (BiomarkerPage string) -> "/biomarkers/" ++ string
--    BottomLevel (ObservationPage string) -> "/results/" ++ string
--urlToPage pathname =
--    let
--        pageParser =
--            oneOf
--                [ format Notes (s "notes" </> int)
--                , format (toPage BottomLevel CategoryPage) (s "biomarker-types" </> string)
--                , format (toPage BottomLevel BiomarkerPage) (s "biomarkers" </> string)
--                , format (toPage BottomLevel ObservationPage) (s "results" </> string)
--                , format (TopLevel PublicationsPage) (s "publications")
--                , format (TopLevel CategoriesPage) (s "biomarker-types")
--                , format (TopLevel BiomarkersPage) (s "biomarkers")
--                , format (TopLevel ObservationsPage) (s "results")
--                , format HomePage (s "")
--                , format DownloadsPage (s "downloads")
--                , format AboutPage (s "about")
--                ]
--    in
--        -- Drop first "/" in pathname. (Otherwise parser doesn't work.)
--        parse identity pageParser (String.dropLeft 1 pathname)
--urlToPage pathname =
--    let
--        route =
--            oneOf
--                [ map Notes (s "notes")
--                , map Lists (s "lists")
--                , map Habits (s "habits")
--                , map Plans (s "plans")
--                , map Settings (s "settings")
--                , map Home (s "")
--                ]
--    in
--        -- Drop first "/" in pathname. (Otherwise parser doesn't work.)
--        parsePath route pathname
--(String.dropLeft 1 pathname)


getKeyFromCode : Int -> Key
getKeyFromCode code =
    case code of
        27 ->
            Esc

        37 ->
            Left

        39 ->
            Right

        40 ->
            Down

        38 ->
            Up

        13 ->
            Enter

        _ ->
            Other


message : msg -> Cmd msg
message x =
    Task.perform identity
        (Task.succeed x)



-- sends new message, which then uses port to actually save model


setLocalStorage =
    message SaveModelLocally


runDateTask =
    Task.perform SetDate Date.now


runTimeTask =
    Task.perform Tick Time.now


zipWith f xs =
    List.map (\x -> ( x, f x )) xs


viewError message =
    div
        [ class "error" ]
        [ text "That's odd. "
        , text message
        , text "(Feel free to yell at me on "
        , a [ href "https   ://twitter.com/dela3499" ] [ text "Twitter" ]
        , text ")."
        ]


viewWeekday date =
    case TimeDate.weekday date of
        TimeDate.Mon ->
            "Mon"

        TimeDate.Tue ->
            "Tue"

        TimeDate.Wed ->
            "Wed"

        TimeDate.Thu ->
            "Thu"

        TimeDate.Fri ->
            "Fri"

        TimeDate.Sat ->
            "Sat"

        TimeDate.Sun ->
            "Sun"


formatHour t =
    case (t % 24) of
        0 ->
            "12 am"

        1 ->
            "1 am"

        2 ->
            "2 am"

        3 ->
            "3 am"

        4 ->
            "4 am"

        5 ->
            "5 am"

        6 ->
            "6 am"

        7 ->
            "7 am"

        8 ->
            "8 am"

        9 ->
            "9 am"

        10 ->
            "10 am"

        11 ->
            "11 am"

        12 ->
            "12 pm"

        13 ->
            "1 pm"

        14 ->
            "2 pm"

        15 ->
            "3 pm"

        16 ->
            "4 pm"

        17 ->
            "5 pm"

        18 ->
            "6 pm"

        19 ->
            "7 pm"

        20 ->
            "8 pm"

        21 ->
            "9 pm"

        22 ->
            "10 pm"

        23 ->
            "11 pm"

        _ ->
            "?"


initModel : Model
initModel =
    { today = Nothing
    , time = 0
    , timer = -10 * 1000
    , menuOpen = False
    , page = Plans
    , activeNote = 0
    , notesFullscreen = False
    , notes =
        List.range 0 4
            |> zipWith (\noteIndex -> { id = noteIndex, title = "", content = "" })
            |> Dict.fromList
    , listGroup = 0
    , listGroups =
        Dict.fromList
            [ ( 0, "career" )
            , ( 1, "hobbies" )
            , ( 2, "learning" )
            , ( 3, "film" )
            , ( 4, "relationships" )
            ]
    , lists =
        listIDs
            |> List.map (\id -> ( id, makeList id ))
            |> Dict.fromList
    , habits =
        [ "daily", "weekly", "monthly" ]
            |> List.map
                (\habitType ->
                    List.range 0 4
                        |> List.map (\habitIndex -> ( habitType, habitIndex ))
                )
            |> List.concat
            |> List.map (\habitID -> ( habitID, makeHabit habitID ))
            |> Dict.fromList
    , tasks = Dict.empty
    , actualSchedule = Dict.empty
    , templateSchedule = Dict.empty
    , scribble = ""
    }


listIDs =
    let
        nGroups =
            5

        nLists =
            6
    in
        List.range 0 (nGroups - 1)
            |> List.map
                (\groupIndex ->
                    List.range 0 (nLists - 1)
                        |> List.map (\listIndex -> ( groupIndex, listIndex ))
                )
            |> List.concat


makeList listID =
    { id = listID
    , title = ""
    , item1 = ""
    , item2 = ""
    , item3 = ""
    }


makeHabit id =
    { id = id
    , title = ""
    , periods = Dict.empty
    }
