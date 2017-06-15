module Homepage.Model exposing (..)

import Date exposing (Day)
import DictList exposing (DictList)


type alias Model =
    { contacts : DictListContact
    , filterString : String
    }


emptyModel : Model
emptyModel =
    { contacts = DictList.empty
    , filterString = ""
    }


type Msg
    = HandleItems (Result String DictListItems)


type alias Contact =
    { name : Name
    , jobTitle : Maybe String
    , imageUrl : Maybe String
    , topics : Maybe (List Topic)
    , phone : Maybe String
    , fax : Maybe String
    , email : Maybe String
    , address : Maybe String
    , receptionTimes : Maybe (List ReceptionTimes)
    }


type alias DictListContact =
    DictList ContactId Contact
