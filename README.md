# alex-tan/postgrest-queries

Easily construct [Postgrest queries](http://postgrest.org/en/v5.1/api.html#horizontal-filtering-rows) using Elm.

```elm
import Postgrest.Queries as P

[ P.select
  [ P.attribute "id"
  , P.attribute "name"
  , P.resourceWithParams "children"
    [ P.order [P.asc "name"] ]
    [ P.attribute "id"
    , P.attribute "name"
    ] 
  ]
]
|> P.toQueryString
-- select=id,name,children(id,name)&children.order=name.asc

queryParams : String
queryParams =
  [ P.limit 10
  , P.order <| P.asc "foo_level"
  , selection
  ]
  |> P.toQueryString
  -- limit=10&order=foo_level.asc&select=id,foo_level,bars(id,name)
```