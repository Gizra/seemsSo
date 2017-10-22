module Currency.Decoder exposing (decodeCurrency)

import Currency.Model exposing (Currency(..))
import Json.Decode as Json exposing (at, andThen, Decoder, fail, int, list, map, nullable, string, succeed)


decodeCurrency : Decoder Currency
decodeCurrency =
    string
        |> andThen
            (\currency ->
                case currency of
                    "EUR" ->
                        succeed EUR

                    "ILS" ->
                        succeed ILS

                    "USD" ->
                        succeed USD

                    _ ->
                        fail <| "Could not recognise currency: " ++ currency
            )
