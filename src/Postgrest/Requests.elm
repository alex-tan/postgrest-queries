module Postgrest.Requests exposing
    ( Error(..)
    , endpoint
    , expectJson
    , expectWhatever
    , jsonResolver
    , patchByPrimaryKey
    , resolution
    )

import Dict exposing (Dict)
import Http exposing (Resolver, header, request, task)
import Json.Decode exposing (Decoder, decodeString, field, list, map, oneOf, string)
import Json.Encode exposing (Value)
import Postgrest.Queries as P
import Task exposing (Task)


endpoint : Endpoint ( Int, String )
endpoint =
    { urlPrefix = Just "/rest"
    , table = "forms"
    , primaryKeyToParams =
        [ ( "commit_id", P.int << Tuple.first )
        , ( "path", P.string << Tuple.second )
        ]
    }


type alias PrimaryKeyConverter primaryKey =
    ( String, primaryKey -> P.Value )


primaryKeyEqClause : List (PrimaryKeyConverter primaryKey) -> primaryKey -> P.Param
primaryKeyEqClause definition pk =
    let
        base =
            definition
                |> List.map
                    (\( key, toParam ) ->
                        P.param key <| P.eq <| toParam pk
                    )
    in
    case base of
        x :: [] ->
            x

        _ ->
            P.and base


type alias Endpoint primaryKey =
    { table : String
    , urlPrefix : Maybe String
    , primaryKeyToParams : List ( String, primaryKey -> P.Value )
    }


patchByPrimaryKey : Endpoint primaryKey -> PatchOptions a -> primaryKey -> Request a
patchByPrimaryKey { table, urlPrefix, primaryKeyToParams } options primaryKey =
    ( Table table
    , Patch
        { body = options.body
        , params =
            P.combineParams
                options.params
                [ primaryKeyEqClause primaryKeyToParams primaryKey ]
        , decoder = options.decoder
        }
    )


deleteByPrimaryKey : Endpoint primaryKey -> DeleteOptions a -> primaryKey -> Request primaryKey
deleteByPrimaryKey { table, urlPrefix, primaryKeyToParams } options primaryKey =
    ( Table table
    , Delete
        { returning = primaryKey
        , params = [ primaryKeyEqClause primaryKeyToParams primaryKey ]
        }
    )


post table options =
    ( Table table, Post options )


unsafePatch : String -> PatchOptions a -> Request a
unsafePatch table options =
    ( Table table
    , Patch options
    )


type alias JWT =
    String


type alias Request a =
    ( Location, RequestType a )


type RequestType a
    = Post (PostOptions a)
    | Patch (PatchOptions a)
    | Get (GetOptions a)
    | Delete (DeleteOptions a)


type alias PostOptions a =
    { body : Value
    , decoder : Decoder a
    , params : P.Params
    }


type alias PatchOptions a =
    { body : Value
    , decoder : Decoder a
    , params : P.Params
    }


type alias GetOptions a =
    { decoder : Decoder a
    , params : P.Params
    }


type alias DeleteOptions a =
    { returning : a
    , params : P.Params
    }


type Location
    = Table String
    | Url String


type alias PrimaryKeyTranslate a =
    Dict String (a -> P.Value)


primaryKeyEq : PrimaryKeyTranslate a -> a -> P.Param
primaryKeyEq def id =
    def
        |> Dict.toList
        |> List.map
            (\( param, toValue ) ->
                P.param param <| P.eq <| toValue id
            )
        |> P.and


toCmd : (Result Error a -> msg) -> JWT -> Request a -> Cmd msg
toCmd toMsg jwt ( table, requestType ) =
    let
        url =
            urlForLocation table
    in
    case requestType of
        Post { body, decoder, params } ->
            request
                { method = "POST"
                , headers = [ jwtHeader jwt, returnRepresentationHeader ]
                , url = url params
                , body = Http.jsonBody body
                , expect = expectJson toMsg decoder
                , timeout = defaultTimeout
                , tracker = Nothing
                }

        Get { decoder, params } ->
            request
                { method = "GET"
                , headers = [ jwtHeader jwt ]
                , url = url params
                , body = Http.emptyBody
                , expect = expectJson toMsg decoder
                , timeout = defaultTimeout
                , tracker = Nothing
                }

        Patch { body, decoder, params } ->
            request
                { method = "PATCH"
                , headers = [ jwtHeader jwt, returnRepresentationHeader ]
                , url = url params
                , body = Http.jsonBody body
                , expect = expectJson toMsg decoder
                , timeout = defaultTimeout
                , tracker = Nothing
                }

        Delete { returning, params } ->
            request
                { method = "DELETE"
                , headers = [ jwtHeader jwt ]
                , url = url params
                , body = Http.emptyBody
                , expect = expectWhatever (toMsg << Result.map (always returning))
                , timeout = defaultTimeout
                , tracker = Nothing
                }


toTask : JWT -> Request a -> Task Error a
toTask jwt ( table, requestType ) =
    let
        url =
            urlForLocation table
    in
    case requestType of
        Post { body, decoder, params } ->
            task
                { method = "POST"
                , headers = [ jwtHeader jwt, returnRepresentationHeader ]
                , url = url params
                , body = Http.jsonBody body
                , resolver = jsonResolver decoder
                , timeout = defaultTimeout
                }

        Get { decoder, params } ->
            task
                { method = "GET"
                , headers = [ jwtHeader jwt ]
                , url = url params
                , body = Http.emptyBody
                , resolver = jsonResolver decoder
                , timeout = defaultTimeout
                }

        Patch { body, decoder, params } ->
            task
                { method = "PATCH"
                , headers = [ jwtHeader jwt, returnRepresentationHeader ]
                , url = url params
                , body = Http.jsonBody body
                , resolver = jsonResolver decoder
                , timeout = defaultTimeout
                }

        Delete { returning, params } ->
            task
                { method = "DELETE"
                , headers = [ jwtHeader jwt ]
                , url = url params
                , body = Http.emptyBody
                , resolver = Http.stringResolver (always <| Ok returning)
                , timeout = defaultTimeout
                }


urlForLocation : Location -> P.Params -> String
urlForLocation loc params =
    let
        base =
            case loc of
                Table table ->
                    "/rest/" ++ table

                Url u ->
                    u
    in
    base ++ "?" ++ P.toQueryString params


returnRepresentationHeader : Http.Header
returnRepresentationHeader =
    header "Prefer" "return=representation"


defaultTimeout : Maybe Float
defaultTimeout =
    Nothing


jwtHeader : JWT -> Http.Header
jwtHeader jwt =
    Http.header "Authorization" <| "Bearer " ++ jwt


jsonResolver : Decoder a -> Resolver Error a
jsonResolver =
    Http.stringResolver << resolution


resolution : Decoder a -> Http.Response String -> Result Error a
resolution decoder response =
    case response of
        Http.BadUrl_ url ->
            Err <| BadUrl url

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            if metadata.statusCode == 404 then
                Err NotFound

            else
                case Json.Decode.decodeString errorDecoder body of
                    Ok errors ->
                        Err <| Errors metadata.statusCode errors

                    Err _ ->
                        Err <| BadStatus metadata.statusCode

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err <| BadBody <| Json.Decode.errorToString err


errorDecoder : Decoder (List String)
errorDecoder =
    oneOf
        [ field "errors" (list string)
        , field "message" string |> map (\m -> [ m ])
        ]


type Error
    = Errors Int (List String)
    | NotFound
    | Timeout
    | BadUrl String
    | NetworkError
    | BadStatus Int
    | BadBody String


expectJson : (Result Error a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg (resolution decoder)


expectWhatever : (Result Error () -> msg) -> Http.Expect msg
expectWhatever toMsg =
    let
        resolve : (body -> Result String a) -> Http.Response body -> Result Error a
        resolve toResult response =
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    Result.mapError BadBody (toResult body)
    in
    Http.expectStringResponse toMsg (resolve (\_ -> Ok ()))
