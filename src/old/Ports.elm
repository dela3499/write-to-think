port module Ports exposing (..)


port urlUpdate : (String -> msg) -> Sub msg


port setLocalStorage : String -> Cmd msg


port setFocus : String -> Cmd msg
