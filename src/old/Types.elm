module Types exposing (..)

import Dict exposing (Dict)
import Date
import Time.Date as TimeDate
import Time exposing (Time)
import Keyboard


-- need to fix page types - they're getting nonsensical


type Msg
    = SetDate Date.Date
    | SetPage Page
    | NotesPage NotesPageMsg
    | ListsPage ListsPageMsg
    | HabitsPage HabitsPageMsg
    | PlansPage PlansPageMsg
    | Tick Time
    | SaveModelLocally
    | UrlUpdate String
    | SetTimer Time
    | OpenMenu
    | CloseMenu
    | NoMsg
    | SetScribble String
    | Menu MenuMsg


type MenuMsg
    = GoToNote Int
    | GoToListGroup Int
    | GoToPlans
    | GoToHabits


type PlansPageMsg
    = SetTask String String
    | SetActual String String
    | SetTemplate String String


type NotesPageMsg
    = SetActiveNote Int
    | SetTitle String
    | SetContent String
    | SetFullscreen
    | SetNormalScreen


type ListsPageMsg
    = SetListGroup Int
    | SetGroupTitle String
    | SetListTitle ( Int, Int ) String
    | SetListItem1 ( Int, Int ) String
    | SetListItem2 ( Int, Int ) String
    | SetListItem3 ( Int, Int ) String


type HabitsPageMsg
    = SetHabitTitle HabitID String
    | TogglePeriod HabitID String


type Page
    = Notes
    | Lists
    | Habits
    | Plans
    | Settings
    | Home


type Key
    = Esc
    | Left
    | Right
    | Down
    | Up
    | Enter
    | Other



-- maybe int-indexed dicts should be arrays?


type alias Model =
    { today : Maybe TimeDate.Date
    , time : Time
    , timer : Time
    , menuOpen : Bool
    , scribble : String
    , activeNote : Int
    , notesFullscreen : Bool
    , page : Page
    , notes : Dict Int Note
    , listGroup : Int
    , listGroups : Dict Int String
    , lists : Dict ( Int, Int ) MyList
    , habits : Dict HabitID Habit
    , tasks : Dict String String
    , actualSchedule : Dict String String
    , templateSchedule : Dict String String
    }



-- omits certain fields relevant only to view state


type alias RawStoredModel =
    { notes : List Note -- need to create dict with id as key
    , listGroups : List ( String, String ) -- convert to dict with int keys
    , lists : List MyList
    , habits : List Habit
    , tasks : Dict String String -- use date string
    , actualSchedule : Dict String String
    , templateSchedule : Dict String String
    }


type alias Note =
    { id : Int
    , title : String
    , content : String
    }


type alias MyList =
    { id : ( Int, Int )
    , title : String
    , item1 : String
    , item2 : String
    , item3 : String
    }


type alias HabitID =
    ( String, Int )


type alias Habit =
    { id : HabitID
    , title : String
    , periods : Dict String Bool
    }


type alias PeriodInfo =
    { date : TimeDate.Date
    , title : String
    , description : String
    , id : String
    }



-- use string instead of tuple3 to store dates
