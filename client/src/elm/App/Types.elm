module App.Types
    exposing
        ( BackendUrl(..)
        , Page(..)
        )

import Backend.Entities exposing (ItemId)
import StorageKey exposing (StorageKey)


type Page
    = Item (StorageKey ItemId)
    | HomePage
    | NotFound


type BackendUrl
    = BackendUrl String
