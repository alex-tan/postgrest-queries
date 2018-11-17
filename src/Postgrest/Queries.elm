module Postgrest.Queries exposing
    ( PostgrestParam, PostgrestParams, PostgrestSelectable, ColumnOrder
    , select
    , allAttributes
    , attribute
    , attributes
    , resource
    , combineParams
    , normalizeParams
    , toQueryString
    , param
    , eq
    , gt
    , gte
    , inList
    , limit
    , lt
    , lte
    , neq
    , or
    , and
    , true
    , false
    , value
    , string
    , int
    , list
    , order
    , asc
    , desc
    )

{-|


# Types

@docs PostgrestParam, PostgrestParams, PostgrestSelectable, ColumnOrder


# Select

@docs select
@docs allAttributes
@docs attribute
@docs attributes
@docs resource


# Converting/combining into something usable

@docs combineParams
@docs normalizeParams
@docs toQueryString


# Param

@docs param


# Conditions

@docs eq
@docs gt
@docs gte
@docs inList
@docs limit
@docs lt
@docs lte
@docs neq
@docs or
@docs and
@docs true
@docs false
@docs value


# Values

@docs string
@docs int
@docs list


# Order

@docs order
@docs asc
@docs desc

-}

import Dict exposing (Dict)
import Url


type alias Params =
    List ( String, String )


{-| A collection of parameters that make up a postgrest request.
-}
type alias PostgrestParams =
    List PostgrestParam


{-| An individual postgrest parameter.
-}
type PostgrestParam
    = Param String PostgrestClause
    | Select (List PostgrestSelectable)
    | Limit Int
    | Order ColumnOrder
    | Or (List PostgrestParam)
    | And (List PostgrestParam)


{-| Join multiple conditions together with or.
-}
or : List PostgrestParam -> PostgrestParam
or =
    Or


{-| Join multiple conditions together with and.
-}
and : List PostgrestParam -> PostgrestParam
and =
    And


{-| A constructor for an individual postgrest parameter.

    param "name" (eq (string "John"))

-}
param : String -> PostgrestClause -> PostgrestParam
param =
    Param


{-| A constructor for the select parameter.

    P.select
        [ P.attribute "id"
        , P.attribute "title"
        , P.resource "user" <|
            P.attributes
                [ "email"
                , "name"
                ]
        ]

-}
select : List PostgrestSelectable -> PostgrestParam
select =
    Select


{-| A constructor for the limit parameter.

    limit 10

-}
limit : Int -> PostgrestParam
limit =
    Limit


{-| A constructor for the limit parameter.

    order (asc "name")

    order (desc "name")

-}
order : ColumnOrder -> PostgrestParam
order =
    Order


{-| Strings, ints and lists need to be normalized into postgrest values
so that the library can format them correctly in our queries.
-}
type PostgrestValue
    = String String
    | Int Int
    | List (List PostgrestValue)


{-| Normalize a string into a postgrest value.
-}
string : String -> PostgrestValue
string =
    String


{-| Normalize an int into a postgrest value.
-}
int : Int -> PostgrestValue
int =
    Int


{-| A type to specify whether you want an order to be ascending or descending.
-}
type ColumnOrder
    = Asc String
    | Desc String


{-| Used in combination with `order` to sort results ascending.
-}
asc : String -> ColumnOrder
asc =
    Asc


{-| Used in combination with `order` to sort results descending.
-}
desc : String -> ColumnOrder
desc =
    Desc


{-| A type that represents the clause of a query. In `name=eq.John` the clause would be the part
after the equal sign.
-}
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
    | False


{-| Used to indicate you need a column to be equal to a certain value.
-}
eq : PostgrestValue -> PostgrestClause
eq =
    Eq


{-| Used to indicate you need a column to be not equal to a certain value.
-}
neq : PostgrestValue -> PostgrestClause
neq =
    Neq


{-| Used to indicate you need a column to be less than a certain value.
-}
lt : Float -> PostgrestClause
lt =
    LT


{-| Used to indicate you need a column to be greater than a certain value.
-}
gt : Float -> PostgrestClause
gt =
    GT


{-| Used to indicate you need a column to be less than or equal than a certain value.
-}
lte : Float -> PostgrestClause
lte =
    LTE


{-| Used to indicate you need a column to be greater than or equal than a certain value.
-}
gte : Float -> PostgrestClause
gte =
    GTE


{-| Used to indicate you need a column to be within a certain list of values.
-}
inList : (a -> PostgrestValue) -> List a -> PostgrestClause
inList toPostgrestValue l =
    In <| List <| List.map toPostgrestValue l


{-| When you don't want to use a specific type after the equals sign in the query, you
can use `value` to set anything you want.
-}
value : PostgrestValue -> PostgrestClause
value =
    Value


{-| When you need a column value to be true
-}
true : PostgrestClause
true =
    True


{-| When you need a column value to be false
-}
false : PostgrestClause
false =
    True


type alias ResourceName =
    String


{-| A type representing which attributes and resources you want to select.
-}
type PostgrestSelectable
    = Attribute String
    | Resource ResourceName (List PostgrestSelectable)


{-| When you want to select a certain column.
-}
attribute : String -> PostgrestSelectable
attribute =
    Attribute


{-| When you want to select a nested resource.
-}
resource : ResourceName -> List PostgrestSelectable -> PostgrestSelectable
resource =
    Resource


{-| This is available if you need it, but more likely you'll want to use
`inList`.
-}
list : List PostgrestValue -> PostgrestValue
list values =
    List values


{-| Shorthand for attributes, when you don't need to specify nested resources:

    -- Short version
    attributes [ "id" "name" ]

    -- Long version
    [ attribute "id"
    , attribute "name"
    ]

-}
attributes : List String -> List PostgrestSelectable
attributes =
    List.map Attribute


{-| When you want to select all attributes. This is only useful when used
to select attributes of a resource or override default parameters in another function
since postgrest returns all attributes by default.
-}
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

        Or _ ->
            "or"

        And _ ->
            "and"

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

        Or conditions ->
            conditions
                |> List.map postgrestParamValue
                |> String.join ","
                |> surroundInParens

        And conditions ->
            conditions
                |> List.map postgrestParamValue
                |> String.join ","
                |> surroundInParens

        Order o ->
            case o of
                Asc field ->
                    field ++ ".asc"

                Desc field ->
                    field ++ ".desc"


surroundInParens : String -> String
surroundInParens s =
    "(" ++ s ++ ")"


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

        False ->
            "is.false"

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


{-| Takes PostgrestParams and returns the parameters as a list of (Key, Value) strings.
-}
normalizeParams : PostgrestParams -> List ( String, String )
normalizeParams =
    List.map (\p -> ( postgrestParamKey p, postgrestParamValue p ))


{-| Takes a default set of params and a custom set of params and prefers the second set.
Useful when you're constructing reusable functions that make similar queries.
-}
combineParams : PostgrestParams -> PostgrestParams -> PostgrestParams
combineParams defaults override =
    Dict.union
        (dictifyParams override)
        (dictifyParams defaults)
        |> Dict.values


paramToString : ( String, String ) -> String
paramToString ( k, v ) =
    k ++ "=" ++ v


{-| Takes PostgrestParams and returns a query string such as
`foo=eq.bar&baz=is.true`
-}
toQueryString : PostgrestParams -> String
toQueryString =
    normalizeParams
        >> List.map paramToString
        >> String.join "&"
