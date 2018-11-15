# alex-tan/postgrest-queries

Easily construct and reuse postgrest queries using Elm.

```elm
import Postgrest.Queries as P

postgrestGet :
    P.PostgrestParams
    -> Decoder a
    -> String
    -> P.PostgrestParams
    -> ApiConfig
    -> Request a
postgrestGet defaultParams decoder resource overrideParams =
    let
        combinedParams =
            combineParams defaultParams overrideParams
                |> P.normalizeParams
    in
    get (queryURL ("/api/" ++ resource) combinedParams) decoder

fooRequest =
  postgrestGet
    [ P.select
      [ P.attribute "id"
      , P.attribute "name"
      , P.attribute "foo_level"
      , P.resource "bars"
        [ P.attribute "id"
        , P.attribute "name"
        ] 
      ]
    ]
    decodeFoos
    "foos"

specificFooRequest =
  fooRequest [P.limit 10, P.order "foo_level"]
```