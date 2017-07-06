module Utils.WebData exposing (getError, sendWithHandler, viewError)

import Json.Decode exposing (Decoder)
import Html exposing (..)
import Http
import HttpBuilder exposing (..)
import RemoteData exposing (..)


{-| Provide some `Html` to view an error message.
-}
viewError : Http.Error -> Html any
viewError error =
    case error of
        Http.BadUrl message ->
            div [] [ text "ErrorBadUrl" ]

        Http.BadPayload message _ ->
            div []
                [ p [] [ text "ErrorBadPayload" ]
                , p [] [ text message ]
                ]

        Http.NetworkError ->
            div [] [ text "ErrorNetworkError" ]

        Http.Timeout ->
            div [] [ text "ErrorTimeout" ]

        Http.BadStatus response ->
            div []
                [ p []
                    [ text "ErrorBadStatus"
                    , p [] [ text response.status.message ]
                    ]
                ]


whenSuccess : RemoteData e a -> result -> (a -> result) -> result
whenSuccess remoteData default func =
    case remoteData of
        Success val ->
            func val

        _ ->
            default


sendWithHandler : Decoder a -> (Result Http.Error a -> msg) -> RequestBuilder a1 -> Cmd msg
sendWithHandler decoder tagger builder =
    builder
        |> withExpect (Http.expectJson decoder)
        |> send tagger


getError : RemoteData e a -> Maybe e
getError remoteData =
    case remoteData of
        Failure err ->
            Just err

        _ ->
            Nothing
