module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Basic.Articles as Articles exposing (articles)
import Resource


type alias Model =
    { articles : Articles.Model }


type Msg
    = ArticlesMsg Articles.Msg


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
    div []
        [ Html.node "link" [ rel "stylesheet", href "/assets/basic.css" ] []
        , Html.map ArticlesMsg (Resource.view articles model.articles)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
