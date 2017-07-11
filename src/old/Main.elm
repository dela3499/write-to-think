module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Json
import Task
import String
import Types exposing (..)
import Update
import View
import Util
import Time
import Decode
import Ports
import Keyboard
import Char


-- setfocus on every page/tab change
-- files, models/types, updates


main =
    Html.programWithFlags
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }


initCmd =
    Cmd.batch
        [ Util.runDateTask
        , Util.runTimeTask
        ]


init : { model : String } -> ( Model, Cmd Msg )
init flags =
    case Decode.jsonToStoredModel flags.model of
        Err _ ->
            ( Util.initModel, initCmd )

        Ok storedModel ->
            ( storedModelToModel storedModel, initCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.second / 4) Tick
        , Ports.urlUpdate (\url -> UrlUpdate url)

        --, Keyboard.downs (keyToMsg model)
        , Keyboard.downs (null model)
        ]



--weird name (should probably change it)


null model keycode =
    let
        key =
            Util.getKeyFromCode keycode
    in
        case ( model.menuOpen, key ) of
            ( False, Esc ) ->
                OpenMenu

            ( True, Esc ) ->
                CloseMenu

            _ ->
                NoMsg



--keyToMsg model keycode =
--    case ( model.menuOpen, Util.getKeyFromCode (Char.fromCode keyCode) ) of
--        ( False, Esc ) ->
--            OpenMenu
--        ( True, Esc ) ->
--            CloseMenu
--        _ ->
--            NoMsg
-- will have a nice menu that allows you to control everything with arrows and stuff. Should be nice!
--keyToMsg keyCode =
--    case (model.menuState, Util.getKeyFromCode keyCode) of
--        (Closed, Esc) -> Menu Open
--        (Closed, _) -> NoMsg
--        (Open _, Esc) -> Menu Close
--        (Open ())
--            case
--        Esc -> ToggleMenu
--        Left ->
--        Right ->
--        Down ->
--        Up ->
--        Enter ->
--        Other ->
-- will eventually be based on url (e.g. page = habits)


storedModelToModel storedModel =
    { notes = storedModel.notes
    , listGroups = storedModel.listGroups
    , lists = storedModel.lists
    , habits = storedModel.habits
    , tasks = storedModel.tasks
    , actualSchedule = storedModel.actualSchedule
    , templateSchedule = storedModel.templateSchedule
    , today = Nothing
    , activeNote = 0
    , notesFullscreen = False
    , page = Notes
    , listGroup = 1
    , time = 0
    , timer = -10 * 1000
    , menuOpen = True
    , scribble = ""
    }
