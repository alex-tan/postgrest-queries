module Postgrest.Queries exposing
    ( PostgrestParam, PostgrestParams, PostgrestSelectable, ColumnOrder
    , Operator
    , select
    , allAttributes
    , attribute
    , attributes
    , resource
    , combineParams
    , normalizeParams
    , toQueryString
    , param
    , or
    , and
    , nestedParam
    , eq
    , gt
    , gte
    , inList
    , limit
    , lt
    , lte
    , neq
    , not
    , true
    , false
    , null
    , value
    , offset
    , ilike
    , like
    , string
    , int
    , list
    , order
    , asc
    , desc
    , nullsfirst
    , nullslast
    , plfts
    , phfts
    , fts
    )

{-|


# Types

@docs PostgrestParam, PostgrestParams, PostgrestSelectable, ColumnOrder
@docs Operator


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
@docs or
@docs and
@docs nestedParam


# Operators

@docs eq
@docs gt
@docs gte
@docs inList
@docs limit
@docs lt
@docs lte
@docs neq
@docs not
@docs true
@docs false
@docs null
@docs value
@docs offset
@docs ilike
@docs like


# Values

@docs string
@docs int
@docs list


# Order

@docs order
@docs asc
@docs desc
@docs nullsfirst
@docs nullslast


# Full-Text Search

@docs plfts
@docs phfts
@docs fts

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
    = Param String Operator
    | NestedParam String PostgrestParam
    | Select (List PostgrestSelectable)
    | Limit Int
    | Offset Int
    | Order (List ColumnOrder)
    | Or (List PostgrestParam)
    | And (List PostgrestParam)


{-| When you want to specify an operator for a nested resource.

    [ select
        [ attribute "*"
        , resource "actors" allAttributes
        ]
    , nestedParam "actors" <| limit 10
    , nestedParam "actors" <| offset 2
    ]
    |> toQueryString
    -- "select=*,actors(*)&actors.limit=10&actors.offset=2"

-}
nestedParam : String -> PostgrestParam -> PostgrestParam
nestedParam =
    NestedParam


{-| Negate a condition.
-}
not : Operator -> Operator
not =
    Not


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
param : String -> Operator -> PostgrestParam
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


{-| Offset
-}
offset : Int -> PostgrestParam
offset =
    Offset


{-| A constructor for the limit parameter.

    order (asc "name")

    order (desc "name")

-}
order : List ColumnOrder -> PostgrestParam
order =
    Order


{-| Strings, ints and lists need to be normalized into postgrest values
so that the library can format them correctly in our queries.
-}
type Value
    = String String
    | Int Int
    | List (List Value)


{-| Normalize a string into a postgrest value.
-}
string : String -> Value
string =
    String


{-| Normalize an int into a postgrest value.
-}
int : Int -> Value
int =
    Int


{-| A type to specify whether you want an order to be ascending or descending.
-}
type ColumnOrder
    = Asc String (Maybe NullOption)
    | Desc String (Maybe NullOption)


type NullOption
    = NullsFirst
    | NullsLast


{-| Sort nulls first.

    order [ asc "age" |> nullsfirst ]

-}
nullsfirst : ColumnOrder -> ColumnOrder
nullsfirst o =
    case o of
        Asc s _ ->
            Asc s (Just NullsFirst)

        Desc s _ ->
            Desc s (Just NullsFirst)


{-| Sort nulls last.

    order [ asc "age" |> nullslast ]

-}
nullslast : ColumnOrder -> ColumnOrder
nullslast o =
    case o of
        Asc s _ ->
            Asc s (Just NullsLast)

        Desc s _ ->
            Desc s (Just NullsLast)


{-| Used in combination with `order` to sort results ascending.
-}
asc : String -> ColumnOrder
asc s =
    Asc s Nothing


{-| Used in combination with `order` to sort results descending.
-}
desc : String -> ColumnOrder
desc s =
    Desc s Nothing


{-| A type that represents the clause of a query. In `name=eq.John` the clause would be the part
after the equal sign.
-}
type Operator
    = Eq Value
    | GT Float
    | GTE Float
    | LT Float
    | LTE Float
    | Neq Value
    | Like String
    | Ilike String
    | In Value
    | Null
    | True
    | False
    | Fts (Maybe Language) String
    | Plfts (Maybe Language) String
    | Phfts (Maybe Language) String
      -- | Cs (List Value)
      -- | Cd (List Value)
      -- | Ov Range
      -- | Sl Range
      -- | Sr Range
      -- | Nxr Range
      -- | Nxl Range
      -- | Adj Range
    | Not Operator
    | Value Value


{-| LIKE operator (use \* in place of %)

    param "text" <| like "foo*bar"

-}
like : String -> Operator
like =
    Like


{-| ILIKE operator (use \* in place of %)

    param "text" <| ilike "foo*bar"

-}
ilike : String -> Operator
ilike =
    Ilike


{-| When a value needs to be null

    param "age" <| null

-}
null : Operator
null =
    Null


{-| Full-Text Search using to\_tsquery

    [ param "my_tsv" <| fts (Just "french") "amusant" ]
        |> toQueryString

    "my_tsv=fts(french).amusant"

-}
fts : Maybe Language -> String -> Operator
fts =
    Fts


{-| Full-Text Search using plainto\_tsquery
-}
plfts : Maybe Language -> String -> Operator
plfts =
    Plfts


{-| Full-Text Search using phraseto\_tsquery
-}
phfts : Maybe Language -> String -> Operator
phfts =
    Phfts


type alias Language =
    String


type alias Range =
    ( Value, Value )


{-| Used to indicate you need a column to be equal to a certain value.
-}
eq : Value -> Operator
eq =
    Eq


{-| Used to indicate you need a column to be not equal to a certain value.
-}
neq : Value -> Operator
neq =
    Neq


{-| Used to indicate you need a column to be less than a certain value.
-}
lt : Float -> Operator
lt =
    LT


{-| Used to indicate you need a column to be greater than a certain value.
-}
gt : Float -> Operator
gt =
    GT


{-| Used to indicate you need a column to be less than or equal than a certain value.
-}
lte : Float -> Operator
lte =
    LTE


{-| Used to indicate you need a column to be greater than or equal than a certain value.
-}
gte : Float -> Operator
gte =
    GTE


{-| Used to indicate you need a column to be within a certain list of values.
-}
inList : (a -> Value) -> List a -> Operator
inList toValue l =
    In <| List <| List.map toValue l


{-| When you don't want to use a specific type after the equals sign in the query, you
can use `value` to set anything you want.
-}
value : Value -> Operator
value =
    Value


{-| When you need a column value to be true
-}
true : Operator
true =
    True


{-| When you need a column value to be false
-}
false : Operator
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
list : List Value -> Value
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

        Offset i ->
            "offset"

        Param k _ ->
            k

        Select _ ->
            "select"

        Order _ ->
            "order"

        Or _ ->
            "or"

        And _ ->
            "and"

        NestedParam r param_ ->
            r ++ "." ++ postgrestParamKey param_


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

        Offset i ->
            String.fromInt i

        Order os ->
            os
                |> List.map
                    (\o ->
                        case o of
                            Asc field nullOption ->
                                [ Just field
                                , Just "asc"
                                , stringifyNullOption nullOption
                                ]
                                    |> catMaybes
                                    |> String.join "."

                            Desc field nullOption ->
                                [ Just field
                                , Just "desc"
                                , stringifyNullOption nullOption
                                ]
                                    |> catMaybes
                                    |> String.join "."
                    )
                |> String.join ","

        And c ->
            wrapConditions c

        Or c ->
            wrapConditions c

        NestedParam nestedResource nestedParam_ ->
            postgrestParamValue nestedParam_


stringifyNullOption : Maybe NullOption -> Maybe String
stringifyNullOption =
    Maybe.map
        (\n_ ->
            case n_ of
                NullsFirst ->
                    "nullsfirst"

                NullsLast ->
                    "nullslast"
        )


catMaybes =
    List.filterMap identity


wrapConditions : PostgrestParams -> String
wrapConditions =
    List.map (normalizeParam >> paramToInnerString)
        >> String.join ","
        >> surroundInParens


surroundInParens : String -> String
surroundInParens s =
    "(" ++ s ++ ")"


stringifyClause : Operator -> String
stringifyClause operator =
    case operator of
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

        Null ->
            "is.null"

        LT val ->
            "lt." ++ String.fromFloat val

        LTE val ->
            "lte." ++ String.fromFloat val

        GT val ->
            "gt." ++ String.fromFloat val

        GTE val ->
            "gte." ++ String.fromFloat val

        Not o ->
            "not." ++ stringifyClause o

        Fts lang val ->
            fullTextSearch "fts" lang val

        Like s ->
            "like." ++ (stringifyQuoted <| string s)

        Ilike s ->
            "ilike." ++ (stringifyQuoted <| string s)

        Plfts lang val ->
            fullTextSearch "plfts" lang val

        Phfts lang val ->
            fullTextSearch "phfts" lang val


fullTextSearch operator lang val =
    operator
        ++ (lang
                |> Maybe.map surroundInParens
                |> Maybe.withDefault ""
           )
        ++ "."
        ++ stringifyValue Basics.False (string val)


stringifyUnquoted : Value -> String
stringifyUnquoted =
    stringifyValue Basics.False


stringifyQuoted : Value -> String
stringifyQuoted =
    stringifyValue Basics.True


stringifyValue : Bool -> Value -> String
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
    List.map normalizeParam


normalizeParam : PostgrestParam -> ( String, String )
normalizeParam p =
    ( postgrestParamKey p, postgrestParamValue p )


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


paramToInnerString : ( String, String ) -> String
paramToInnerString ( k, v ) =
    case k of
        "and" ->
            k ++ v

        "or" ->
            k ++ v

        _ ->
            k ++ "." ++ v


{-| Takes PostgrestParams and returns a query string such as
`foo=eq.bar&baz=is.true`
-}
toQueryString : PostgrestParams -> String
toQueryString =
    normalizeParams
        >> List.map paramToString
        >> String.join "&"
