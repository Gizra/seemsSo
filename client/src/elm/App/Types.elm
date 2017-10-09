module App.Types exposing (Page)

import Backend.Entities exposing (ItemId)


type Page
    = Item ItemId
      -- | HomePage
    | NotFound


type BackendUrl
    = BackendUrl String
