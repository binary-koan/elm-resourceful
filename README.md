# Scaffoldry

A framework for creating and updating content in Elm. Tell Scaffoldry
what your data is and where it resides, and it'll wire it up into a UI.

## Usage

```elm
articles : Resource Article Msg
articles =
    { empty = emptyArticle          -- A blank, "new" article
    , update = updateArticle        -- Updates an article based on a Msg
    , store = memoryStore           -- Defines how articles are stored and retrieved
    , render = basicRenderer fields -- Defines how articles are rendered
    , sortWith = \a1 a2 -> compare a1.title a2.title
    }
```

This just stores the articles in memory and renders them as a plain list,
which isn't very useful. However, to start using real data you only need to
swap out the store for one which points to a RESTful API, GraphQL endpoint,
local database or any custom resource format. And if you want the view to
look better, just swap out the renderer for one which suits your existing CSS.
