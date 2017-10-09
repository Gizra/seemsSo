module App.Types
    exposing
        ( BackendUrl(..)
        , Page(..)
        )

import Backend.Entities exposing (ItemId)


type Page
    = Item ItemId
    | HomePage
    | NotFound


type BackendUrl
    = BackendUrl String
