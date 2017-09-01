module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Article =
    { title : String, content : String }


type alias Model =
    { articles : List Article, newArticle : Article }


type Msg
    = SetNewArticleTitle String
    | SetNewArticleContent String
    | CreateNewArticle


emptyArticle : Article
emptyArticle =
    { title = "", content = "" }


init : ( Model, Cmd Msg )
init =
    ( { articles = [], newArticle = emptyArticle }, Cmd.none )


setTitle : String -> Article -> Article
setTitle title article =
    { article | title = title }


setContent : String -> Article -> Article
setContent content article =
    { article | content = content }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewArticleTitle title ->
            ( { model | newArticle = setTitle title model.newArticle }, Cmd.none )

        SetNewArticleContent content ->
            ( { model | newArticle = setContent content model.newArticle }, Cmd.none )

        CreateNewArticle ->
            ( { model | articles = model.newArticle :: model.articles, newArticle = emptyArticle }, Cmd.none )


articleView : Article -> Html Msg
articleView article =
    li []
        [ h2 [] [ text article.title ]
        , p [] [ text article.content ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Articles" ]
        , ul [] (List.map articleView model.articles)
        , Html.form [ onSubmit CreateNewArticle ]
            [ h2 [] [ text "New Article" ]
            , p []
                [ label [] [ text "Title" ]
                , input [ type_ "text", value model.newArticle.title, onInput SetNewArticleTitle ] []
                ]
            , p []
                [ label [] [ text "Content" ]
                , textarea [ value model.newArticle.content, onInput SetNewArticleContent ] []
                ]
            , button [ type_ "submit" ] [ text "Create" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
