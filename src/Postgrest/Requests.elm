module Postgrest.Requests exposing
    ( Endpoint
    , Error(..)
    , JWT
    , Request
    , customURL
    , deleteByPrimaryKey
    , endpoint
    , expectJson
    , expectWhatever
    , getMany
    , getOne
    , jsonResolver
    , jwt
    , patchByPrimaryKey
    , postOne
    , prefixedTable
    , resolution
    , setParams
    , table
    , toCmd
    , toHttpError
    , toTask
    , unsafePatch
    )

import Http exposing (Resolver, header, task)
import Json.Decode exposing (Decoder, decodeString, field, index, list, map, map4, maybe, string)
import Json.Encode exposing (Value)
import Postgrest.Queries as P
import Task exposing (Task)


type alias PrimaryKeyConverter primaryKey =
    ( String, primaryKey -> P.Value )


type JWT
    = JWT String


jwt : String -> JWT
jwt =
    JWT


type Request r
    = Request
        { options : RequestType r
        , timeout : Maybe Float
        , defaultParams : P.Params
        , overrideParams : P.Params
        , mandatoryParams : P.Params
        , baseURL : String
        }


timeout : Request r -> Maybe Float
timeout (Request r) =
    r.timeout


options : Request r -> RequestType r
options (Request r) =
    r.options


defaultRequest : Endpoint p r -> RequestType returning -> Request returning
defaultRequest e requestType =
    Request
        { options = requestType
        , timeout = Nothing
        , defaultParams = endpointToDefaultParams e
        , overrideParams = []
        , mandatoryParams = []
        , baseURL = endpointToBaseURL e
        }


setParams : P.Params -> Request r -> Request r
setParams p (Request req) =
    Request { req | overrideParams = p }


setMandatoryParams : P.Params -> Request r -> Request r
setMandatoryParams p (Request req) =
    Request { req | mandatoryParams = p }


url : Request r -> String
url (Request { defaultParams, overrideParams, mandatoryParams, baseURL }) =
    let
        params =
            P.concatParams [ defaultParams, overrideParams, mandatoryParams ]
    in
    baseURL ++ "?" ++ P.toQueryString params


type RequestType r
    = Post Value (Decoder r)
    | Patch Value (Decoder r)
    | Get (Decoder r)
    | Delete r


getOne : Endpoint p r -> p -> Request r
getOne e primaryKey =
    defaultRequest e (Get <| index 0 e.decoder)
        |> setMandatoryParams [ primaryKeyEqClause e.primaryKeyToParams primaryKey ]


getMany : Endpoint p r -> Request (List r)
getMany e =
    defaultRequest e <| Get <| list e.decoder


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


patchByPrimaryKey : Endpoint pk record -> pk -> Value -> Request record
patchByPrimaryKey e primaryKey body =
    defaultRequest e (Patch body <| index 0 e.decoder)
        |> setMandatoryParams [ primaryKeyEqClause e.primaryKeyToParams primaryKey ]


deleteByPrimaryKey : Endpoint p r -> p -> Request p
deleteByPrimaryKey e primaryKey =
    defaultRequest e (Delete primaryKey)
        |> setMandatoryParams [ primaryKeyEqClause e.primaryKeyToParams primaryKey ]


postOne : Endpoint p r -> Value -> Request r
postOne e body =
    defaultRequest e <| Post body <| index 0 e.decoder


unsafePatch : Endpoint p r -> Value -> Request (List r)
unsafePatch e body =
    defaultRequest e <| Patch body <| list e.decoder


toCmd : JWT -> (Result Error r -> msg) -> Request r -> Cmd msg
toCmd jwt_ toMsg request =
    let
        timeout_ =
            timeout request

        url_ =
            url request
    in
    case options request of
        Post body decoder ->
            Http.request
                { method = "POST"
                , headers = [ jwtHeader jwt_, returnRepresentationHeader ]
                , url = url_
                , body = Http.jsonBody body
                , expect = expectJson toMsg decoder
                , timeout = timeout_
                , tracker = Nothing
                }

        Get decoder ->
            Http.request
                { method = "GET"
                , headers = [ jwtHeader jwt_ ]
                , url = url_
                , body = Http.emptyBody
                , expect = expectJson toMsg decoder
                , timeout = timeout_
                , tracker = Nothing
                }

        Patch body decoder ->
            Http.request
                { method = "PATCH"
                , headers = [ jwtHeader jwt_, returnRepresentationHeader ]
                , url = url_
                , body = Http.jsonBody body
                , expect = expectJson toMsg decoder
                , timeout = timeout_
                , tracker = Nothing
                }

        Delete returning ->
            Http.request
                { method = "DELETE"
                , headers = [ jwtHeader jwt_ ]
                , url = url_
                , body = Http.emptyBody
                , expect = expectWhatever (toMsg << Result.map (always returning))
                , timeout = timeout_
                , tracker = Nothing
                }


toTask : JWT -> Request r -> Task Error r
toTask jwt_ request =
    let
        timeout_ =
            timeout request

        url_ =
            url request
    in
    case options request of
        Post body decoder ->
            task
                { method = "POST"
                , headers = [ jwtHeader jwt_, returnRepresentationHeader ]
                , url = url_
                , body = Http.jsonBody body
                , resolver = jsonResolver decoder
                , timeout = timeout_
                }

        Get decoder ->
            task
                { method = "GET"
                , headers = [ jwtHeader jwt_ ]
                , url = url_
                , body = Http.emptyBody
                , resolver = jsonResolver decoder
                , timeout = timeout_
                }

        Patch body decoder ->
            task
                { method = "PATCH"
                , headers = [ jwtHeader jwt_, returnRepresentationHeader ]
                , url = url_
                , body = Http.jsonBody body
                , resolver = jsonResolver decoder
                , timeout = timeout_
                }

        Delete returning ->
            task
                { method = "DELETE"
                , headers = [ jwtHeader jwt_ ]
                , url = url_
                , body = Http.emptyBody
                , resolver = Http.stringResolver <| always <| Ok returning
                , timeout = timeout_
                }


returnRepresentationHeader : Http.Header
returnRepresentationHeader =
    header "Prefer" "return=representation"


jwtHeader : JWT -> Http.Header
jwtHeader (JWT jwt_) =
    Http.header "Authorization" <| "Bearer " ++ jwt_


jsonResolver : Decoder a -> Resolver Error a
jsonResolver =
    Http.stringResolver << resolution


resolution : Decoder a -> Http.Response String -> Result Error a
resolution decoder response =
    case response of
        Http.BadUrl_ url_ ->
            Err <| BadUrl url_

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err <| badStatusBodyToPostgrestError metadata.statusCode body

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err <| BadBody <| Json.Decode.errorToString err


expectJson : (Result Error a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg (resolution decoder)


expectWhatever : (Result Error () -> msg) -> Http.Expect msg
expectWhatever toMsg =
    let
        resolve : (body -> Result String a) -> Http.Response body -> Result Error a
        resolve toResult response =
            case response of
                Http.BadUrl_ url_ ->
                    Err <| BadUrl url_

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata _ ->
                    Err <| BadStatus metadata.statusCode emptyErrors

                Http.GoodStatus_ _ body ->
                    Result.mapError BadBody (toResult body)
    in
    Http.expectStringResponse toMsg (resolve (always <| Ok ()))


type alias Endpoint primaryKey record =
    { url : URL
    , primaryKeyToParams : List ( String, primaryKey -> P.Value )
    , decoder : Decoder record
    , defaultSelect : Maybe (List P.Selectable)
    , defaultOrder : Maybe (List P.ColumnOrder)
    , defaultLimit : Maybe Int
    }


endpoint : Endpoint pk r -> Endpoint pk r
endpoint =
    identity


endpointToDefaultParams : Endpoint p r -> P.Params
endpointToDefaultParams { defaultSelect, defaultOrder, defaultLimit } =
    [ defaultSelect |> Maybe.map P.select
    , defaultOrder |> Maybe.map P.order
    , defaultLimit |> Maybe.map P.limit
    ]
        |> List.filterMap identity


type URL
    = Table String
    | PrefixedTable String String
    | URL String


table : String -> URL
table =
    Table


prefixedTable : String -> String -> URL
prefixedTable =
    PrefixedTable


customURL : String -> URL
customURL =
    URL


endpointToBaseURL : Endpoint primaryKey record -> String
endpointToBaseURL e =
    case e.url of
        Table tableName ->
            "/" ++ tableName

        PrefixedTable prefix tableName ->
            prefix ++ "/" ++ tableName

        URL u ->
            u


type alias PostgrestErrorJSON =
    { message : Maybe String
    , details : Maybe String
    , hint : Maybe String
    , code : Maybe String
    }


decodePostgrestError : Decoder PostgrestErrorJSON
decodePostgrestError =
    map4 PostgrestErrorJSON
        (maybe (field "message" string))
        (maybe (field "details" string))
        (maybe (field "hint" string))
        (maybe (field "code" string))


emptyErrors : PostgrestErrorJSON
emptyErrors =
    PostgrestErrorJSON
        Nothing
        Nothing
        Nothing
        Nothing


type Error
    = Timeout
    | BadUrl String
    | NetworkError
    | BadStatus Int PostgrestErrorJSON
    | BadBody String


toHttpError : Error -> Http.Error
toHttpError e =
    case e of
        Timeout ->
            Http.Timeout

        BadUrl s ->
            Http.BadUrl s

        NetworkError ->
            Http.NetworkError

        BadStatus i _ ->
            Http.BadStatus i

        BadBody s ->
            Http.BadBody s


badStatusBodyToPostgrestError : Int -> String -> Error
badStatusBodyToPostgrestError statusCode body =
    case Json.Decode.decodeString decodePostgrestError body of
        Ok errors ->
            BadStatus statusCode errors

        Err _ ->
            BadStatus statusCode emptyErrors
