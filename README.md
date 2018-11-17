# alex-tan/postgrest-queries

Easily construct [Postgrest queries](http://postgrest.org/en/v5.1/api.html#horizontal-filtering-rows) using Elm.

```elm
import Postgrest.Queries as P

selection : PostgrestParam
selection =
  P.select
    [ P.attribute "id"
    , P.attribute "foo_level"
    , P.resource "bars"
      [ P.attribute "id"
      , P.attribute "name"
      ] 
    ]

queryParams : String
queryParams =
  [ P.limit 10
  , P.order <| P.asc "foo_level"
  , selection
  ]
  |> P.toQueryString
  -- limit=10&order=foo_level.asc&select=id,foo_level,bars(id,name)
```