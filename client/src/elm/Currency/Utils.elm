module Currency.Utils exposing (..)

import Currency.Model exposing (..)


currencySymbol : Currency -> String
currencySymbol currency =
    case currency of
        EUR ->
            "€"

        ILS ->
            "₪"

        USD ->
            "$"
