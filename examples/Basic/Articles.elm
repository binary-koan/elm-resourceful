module Basic.Articles exposing (articles, Model, Msg)

import Resource exposing (..)
import Resource.Render.Basic exposing (basicRenderer)
import Resource.Store.Memory exposing (memoryStore)


type alias Model =
    Resource.Model Article


type alias Msg =
    Resource.Msg Article ArticleUpdate


type alias Article =
    { title : String, content : String }


type ArticleUpdate
    = SetTitle String
    | SetContent String


articles : Resource Article ArticleUpdate
articles =
    { empty = emptyArticle
    , update = updateArticle
    , store = memoryStore
    , render = basicRenderer fields
    , sortWith = \a1 a2 -> compare a1.title a2.title
    }


emptyArticle : Article
emptyArticle =
    { title = "", content = "" }


fields : Article -> List (Field ArticleUpdate)
fields article =
    [ TextField "Title" article.title SetTitle
    , TextArea "Content" article.content SetContent
    ]


updateArticle : ArticleUpdate -> Article -> Article
updateArticle update article =
    case update of
        SetTitle title ->
            { article | title = title }

        SetContent content ->
            { article | content = content }
