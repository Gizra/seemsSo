module Amount exposing (..)

import Currency.Model exposing (Currency(..))
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Utils.Json exposing (decodeInt)


type Amount
    = Amount Int


decodeAmount : Decoder Amount
decodeAmount =
    Decode.map Amount <|
        decodeInt


encodeAmount : Amount -> Value
encodeAmount (Amount int) =
    Encode.int int


{-| Empty amount type. Useful for starting folds that result in an amount.
-}
zero : Amount
zero =
    Amount 0


{-| Add two amounts together.
-}
add : Amount -> Amount -> Amount
add =
    map2 (+)


{-| Sum a list of amounts.
-}
sum : List Amount -> Amount
sum =
    List.foldl add zero


{-| Way of comparing the value of two amounts.

    >>> compare (/=) (Amount 1) (Amount 2)
    True

-}
compare : (Int -> Int -> Bool) -> Amount -> Amount -> Bool
compare comparator (Amount x) (Amount y) =
    comparator x y


{-| Same as compare, but reorders the arguments to make it a bit more intuitive
for infix operators.

    >>> compareInfix (Amount 1) (<=) (Amount 2)
    True

-}
compareInfix : Amount -> (Int -> Int -> Bool) -> Amount -> Bool
compareInfix x comparator y =
    compare comparator x y


map : (Int -> Int) -> Amount -> Amount
map fn (Amount x) =
    Amount (fn x)


map2 : (Int -> Int -> Int) -> Amount -> Amount -> Amount
map2 fn (Amount x) (Amount y) =
    Amount (fn x y)


{-| Extracts integer from amount.
-}
extract : Amount -> Int
extract (Amount x) =
    x


{-| Amount with proper currency symbol, simple String.
-}
showAmountWithCurrencyText : Amount -> Currency -> String
showAmountWithCurrencyText (Amount x) currency =
    let
        value =
            format { usLocale | decimals = 0 } <| toFloat x
    in
    String.concat <| showWithCurrency identity currency value


{-| Amount with proper currency symbol.
-}
showAmountWithCurrency : Amount -> Currency -> Html a
showAmountWithCurrency amount currency =
    text <|
        showAmountWithCurrencyText amount currency


{-| This allows you to place the currency on the correct side of anything, not
just String. It returns a two-element list with the currency and the other
thing in the correct order -- you just have to provide a function to wrap the
string correctly. E.g. for Html form input,

    showWithCurrency text EUR (input [] []) == [ input [] [], text "€" ]

-}
showWithCurrency : (String -> a) -> Currency -> a -> List a
showWithCurrency wrapper currency value =
    case currency of
        EUR ->
            [ value, wrapper "€" ]

        ILS ->
            [ wrapper "₪", value ]

        USD ->
            [ wrapper "$", value ]
