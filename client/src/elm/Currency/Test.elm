module Currency.Test exposing (all)

import Expect
import Currency.Model exposing (Currency(..))
import Amount exposing (Amount(..), showAmountWithCurrencyText)
import Test exposing (Test)


currencySignsTest : Test
currencySignsTest =
    Test.describe "Decorates amount with currency"
        [ Test.test "Decodes amount with USD" <|
            \() ->
                let
                    amount =
                        Amount 10

                    currency =
                        USD

                    expected =
                        "$10"
                in
                    Expect.equal expected (showAmountWithCurrencyText amount currency)
        , Test.test "Decodes amount with EUR" <|
            \() ->
                let
                    amount =
                        Amount 99

                    currency =
                        EUR

                    expected =
                        "99€"
                in
                    Expect.equal expected (showAmountWithCurrencyText amount currency)
        , Test.test "Decodes amount with ILS" <|
            \() ->
                let
                    amount =
                        Amount -25000000000

                    currency =
                        ILS

                    expected =
                        "₪−25,000,000,000"
                in
                    Expect.equal expected (showAmountWithCurrencyText amount currency)
        ]


all : Test
all =
    Test.describe "Currency tests"
        [ currencySignsTest
        ]
