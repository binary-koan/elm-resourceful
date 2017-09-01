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
    { list : LoadState String (List ( resource, Maybe resource ))
    , new : resource
    , creating : Bool
    }


type LoadState err ok
    = NotRequested
    | Loading
    | Loaded ok
    | Error err


type Msg resource update
    = Update resource update
    | Create


type Field update
    = TextField String String (String -> update)
    | TextArea String String (String -> update)
    | NumberField String Float (Float -> update)
    | CheckboxField String Bool (Bool -> update)


type alias FieldBuilder resource update =
    resource -> List (Field update)


emptyModel : resource -> Model resource
emptyModel new =
    { list = NotRequested
    , new = new
    , creating = False
    }


init : Resource r u -> ( Model r, Cmd (Msg r u) )
init resource =
    ( emptyModel resource.empty, Cmd.none )


update : Resource r u -> Msg r u -> Model r -> ( Model r, Cmd (Msg r u) )
update resource msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Resource r u -> Model r -> Html (Msg r u)
view resource model =
    resource.render model
