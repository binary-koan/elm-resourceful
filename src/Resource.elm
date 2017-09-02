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
    , list : List (Editable resource)
    , nextId : Int
    , new : Editable resource
    , creating : Bool
    }


type ResourceId
    = ExistingId Int
    | NewId


type alias Editable r =
    { id : ResourceId
    , original : r
    , editor : Maybe r
    , validation : Validation
    , saveError : Maybe String
    }


type Validation
    = Pass
    | Fail (List ( String, String ))


type LoadState err
    = NotRequested
    | Loading
    | Loaded
    | Error err


type Msg resource update
    = Edit ResourceId
    | Update ResourceId update
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
emptyModel emptyResource =
    { state = NotRequested
    , list = []
    , nextId = 0
    , new = editableNewResource emptyResource
    , creating = False
    }


editableNewResource : resource -> Editable resource
editableNewResource resource =
    startEditing NewId (editable NewId resource)


init : Resource r u -> ( Model r, Cmd (Msg r u) )
init resource =
    ( emptyModel resource.empty, Cmd.none )


update : Resource r u -> Msg r u -> Model r -> ( Model r, Cmd (Msg r u) )
update resource msg model =
    case msg of
        Update id update ->
            doUpdate id (resource.update update) model ! []

        Create ->
            case model.new.editor of
                Just res ->
                    model ! [ Task.attempt createHandler (resource.store.create res) ]

                Nothing ->
                    model ! []

        CreateSucceeded res ->
            { model | list = (editable (ExistingId model.nextId) res) :: model.list, nextId = model.nextId + 1, new = editableNewResource resource.empty } ! []

        CreateFailed _ ->
            model ! []

        Edit id ->
            { model | list = List.map (startEditing id) model.list } ! []


editable : ResourceId -> r -> Editable r
editable id resource =
    { id = id, original = resource, editor = Nothing, validation = Pass, saveError = Nothing }


startEditing : ResourceId -> Editable r -> Editable r
startEditing id editable =
    if editable.id == id then
        { editable | editor = Just editable.original }
    else
        editable


doUpdate : ResourceId -> (r -> r) -> Model r -> Model r
doUpdate id updater model =
    { model | new = updateEditor id updater model.new, list = List.map (updateEditor id updater) model.list }


updateEditor : ResourceId -> (r -> r) -> Editable r -> Editable r
updateEditor id updater editable =
    if editable.id == id then
        { editable | editor = Maybe.map updater editable.editor }
    else
        editable


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
