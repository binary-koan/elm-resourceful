module Resource exposing (..)

import Task exposing (Task)
import Html exposing (Html)


type alias Resource resource update =
    { empty : resource
    , update : update -> resource -> resource
    , store : Store resource
    , render : Renderer resource update
    }


type alias Store resource =
    { list : Task String (List resource)
    , create : resource -> Task String resource
    }


type alias Renderer resource update =
    Model resource -> Html (Msg resource update)


type alias Model resource =
    { state : LoadState String
    , list : List ( resource, Maybe resource )
    , new : resource
    , creating : Bool
    }


type LoadState err
    = NotRequested
    | Loading
    | Loaded
    | Error err


type Msg resource update
    = Update resource update
    | Create
    | CreateSucceeded resource
    | CreateFailed String


type Field update
    = TextField String String (String -> update)
    | TextArea String String (String -> update)
    | NumberField String Float (Float -> update)
    | CheckboxField String Bool (Bool -> update)


type alias FieldBuilder resource update =
    resource -> List (Field update)


emptyModel : resource -> Model resource
emptyModel new =
    { state = NotRequested
    , list = []
    , new = new
    , creating = False
    }


init : Resource r u -> ( Model r, Cmd (Msg r u) )
init resource =
    ( emptyModel resource.empty, Cmd.none )


update : Resource r u -> Msg r u -> Model r -> ( Model r, Cmd (Msg r u) )
update resource msg model =
    case msg of
        Update _ update ->
            { model | new = resource.update update model.new } ! []

        Create ->
            model ! [ Task.attempt createHandler (resource.store.create model.new) ]

        CreateSucceeded res ->
            { model | list = ( res, Nothing ) :: model.list, new = resource.empty } ! []

        CreateFailed _ ->
            model ! []


createHandler : Result String r -> Msg r u
createHandler result =
    case result of
        Ok res ->
            CreateSucceeded res

        Err err ->
            CreateFailed err


view : Resource r u -> Model r -> Html (Msg r u)
view resource model =
    resource.render model
