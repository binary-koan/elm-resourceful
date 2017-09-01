module Main exposing (..)

import Html exposing (..)
import Articles exposing (..)
import Resource


type alias Model =
    { articles : Articles.Model }


type Msg
    = ArticlesMsg Articles.Msg



-- emptyArticle : Article
-- emptyArticle =
--     { title = "", content = "" }
-- init : ( Model, Cmd Msg )
-- init =
--     ( { articles = [], newArticle = emptyArticle }, Cmd.none )
-- setTitle : String -> Article -> Article
-- setTitle title article =
--     { article | title = title }
-- setContent : String -> Article -> Article
-- setContent content article =
--     { article | content = content }
-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     case msg of
--         SetNewArticleTitle title ->
--             ( { model | newArticle = setTitle title model.newArticle }, Cmd.none )
--         SetNewArticleContent content ->
--             ( { model | newArticle = setContent content model.newArticle }, Cmd.none )
--         CreateNewArticle ->
--             ( { model | articles = model.newArticle :: model.articles, newArticle = emptyArticle }, Cmd.none )
-- articleView : Article -> Html Msg
-- articleView article =
--     li []
--         [ h2 [] [ text article.title ]
--         , p [] [ text article.content ]
--         ]
-- view : Model -> Html Msg
-- view model =
--     div []
--         [ h1 [] [ text "Articles" ]
--         , ul [] (List.map articleView model.articles)
--         , Html.form [ onSubmit CreateNewArticle ]
--             [ h2 [] [ text "New Article" ]
--             , p []
--                 [ label [] [ text "Title" ]
--                 , input [ type_ "text", value model.newArticle.title, onInput SetNewArticleTitle ] []
--                 ]
--             , p []
--                 [ label [] [ text "Content" ]
--                 , textarea [ value model.newArticle.content, onInput SetNewArticleContent ] []
--                 ]
--             , button [ type_ "submit" ] [ text "Create" ]
--             ]
--         ]
-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    let
        ( articlesModel, articlesCmd ) =
            Resource.init articles
    in
        { articles = articlesModel } ! [ Cmd.map ArticlesMsg articlesCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArticlesMsg a ->
            let
                ( articlesModel, articlesCmd ) =
                    Resource.update articles a model.articles
            in
                { model | articles = articlesModel } ! [ Cmd.map ArticlesMsg articlesCmd ]


view : Model -> Html Msg
view model =
    Html.map ArticlesMsg (Resource.view articles model.articles)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
