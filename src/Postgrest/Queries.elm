module Postgrest.Queries exposing
    ( Params
    , PostgrestParam
    , PostgrestParams
    , PostgrestSelectable
    , allAttributes
    , attribute
    , attributes
    , combineParams
    , desc
    , dictifyParams
    , eq
    , gt
    , gte
    , inList
    , int
    , limit
    , lt
    , lte
    , neq
    , normalizeParams
    , order
    , param
    , resource
    , select
    , string
    , true
    , value
    )

import Dict exposing (Dict)
import Url


type alias Params =
    List ( String, String )


type alias PostgrestParams =
    List PostgrestParam


type PostgrestParam
    = Param String PostgrestClause
    | Select (List PostgrestSelectable)
    | Limit Int
    | Order ColumnOrder


param =
    Param


select =
    Select


limit =
    Limit


order =
    Order


type PostgrestValue
    = String String
    | Int Int
    | List (List PostgrestValue)


string =
    String


int =
    Int


type ColumnOrder
    = Asc String
    | Desc String


asc =
    Asc


desc =
    Desc


type PostgrestClause
    = Eq PostgrestValue
    | Neq PostgrestValue
    | LT Float
    | GT Float
    | LTE Float
    | GTE Float
    | In PostgrestValue
    | Value PostgrestValue
    | True


eq =
    Eq


neq =
    Neq


lt =
    LT


gt =
    GT


lte =
    LTE


gte =
    GTE


inList l =
    In <| List l


value =
    Value


true =
    True


type alias ResourceName =
    String


type PostgrestSelectable
    = Attribute String
    | Resource ResourceName (List PostgrestSelectable)


attribute =
    Attribute


resource =
    Resource


list : (a -> PostgrestValue) -> List a -> PostgrestValue
list mapper l =
    List <| List.map mapper l


attributes : List String -> List PostgrestSelectable
attributes =
    List.map Attribute


allAttributes : List PostgrestSelectable
allAttributes =
    attributes [ "*" ]


postgrestParamKey : PostgrestParam -> String
postgrestParamKey p =
    case p of
        Limit i ->
            "limit"

        Param k _ ->
            k

        Select _ ->
            "select"

        Order _ ->
            "order"


postgrestParamValue : PostgrestParam -> String
postgrestParamValue p =
    case p of
        Param _ clause ->
            stringifyClause clause

        Select attrs ->
            attrs
                |> List.map stringifyPostgrestSelect
                |> String.join ","

        Limit i ->
            String.fromInt i

        Order o ->
            case o of
                Asc field ->
                    field ++ ".asc"

                Desc field ->
                    field ++ ".desc"


stringifyClause : PostgrestClause -> String
stringifyClause clause =
    case clause of
        Neq val ->
            "neq." ++ stringifyUnquoted val

        Eq val ->
            "eq." ++ stringifyUnquoted val

        In val ->
            "in.(" ++ stringifyQuoted val ++ ")"

        Value val ->
            stringifyUnquoted val

        True ->
            "is.true"

        LT val ->
            "lt." ++ String.fromFloat val

        LTE val ->
            "lte." ++ String.fromFloat val

        GT val ->
            "gt." ++ String.fromFloat val

        GTE val ->
            "gte." ++ String.fromFloat val


stringifyUnquoted : PostgrestValue -> String
stringifyUnquoted =
    stringifyValue Basics.False


stringifyQuoted : PostgrestValue -> String
stringifyQuoted =
    stringifyValue Basics.True


stringifyValue : Bool -> PostgrestValue -> String
stringifyValue quotes val =
    case val of
        String str ->
            if quotes then
                "\"" ++ Url.percentEncode str ++ "\""

            else
                Url.percentEncode str

        Int i ->
            String.fromInt i

        List l ->
            l
                |> List.map (stringifyValue quotes)
                |> String.join ","


stringifyPostgrestSelect : PostgrestSelectable -> String
stringifyPostgrestSelect postgrestSelect =
    case postgrestSelect of
        Attribute attr ->
            attr

        Resource resourceName attrs ->
            case attrs of
                [] ->
                    resourceName

                _ ->
                    resourceName
                        ++ "("
                        ++ (attrs
                                |> List.map stringifyPostgrestSelect
                                |> String.join ","
                           )
                        ++ ")"


dictifyParams : PostgrestParams -> Dict String PostgrestParam
dictifyParams =
    List.map (\p -> ( postgrestParamKey p, p )) >> Dict.fromList


normalizeParams : PostgrestParams -> List ( String, String )
normalizeParams =
    List.map (\p -> ( postgrestParamKey p, postgrestParamValue p ))


combineParams : PostgrestParams -> PostgrestParams -> PostgrestParams
combineParams defaults override =
    Dict.union
        (dictifyParams override)
        (dictifyParams defaults)
        |> Dict.values
