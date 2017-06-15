port module Main exposing (..)

import Homepage.Test
import Json.Encode exposing (Value)
import Test exposing (Test, describe)
import Test.Runner.Node exposing (TestProgram, run)


allTests : Test
allTests =
    describe "All tests"
        [ Homepage.Test.all
        ]


main : TestProgram
main =
    run emit allTests


port emit : ( String, Value ) -> Cmd msg
