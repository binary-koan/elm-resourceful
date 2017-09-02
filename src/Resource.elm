module Resource exposing (..)

import Task exposing (Task)
import Html exposing (Html)


-- BUILDING


type alias Resource resource update =
    { empty : resource
    , update : update -> resource -> resource
    , store : Store resource
    , render : Renderer resource update
    , sortWith : resource -> resource -> Order
    }


type alias Store resource =
    { list : Task String (List resource)
    , create : resource -> Task String resource
    , update : resource -> Task String resource
    , pageSize : Maybe Int
    }


type alias Renderer resource update =
    Model resource -> Html (Msg resource update)


type Field update
    = TextField String String (String -> update)
    | TextArea String String (String -> update)
    | NumberField String Float (Float -> update)
    | CheckboxField String Bool (Bool -> update)


type alias FieldBuilder resource update =
    resource -> List (Field update)



-- MODEL


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


init : Resource r u -> ( Model r, Cmd (Msg r u) )
init resource =
    emptyModel resource.empty ! []


emptyModel : resource -> Model resource
emptyModel emptyResource =
    { state = NotRequested
    , list = []
    , nextId = 0
    , new = editableNewResource emptyResource
    , creating = False
    }



-- UPDATE


type Msg resource update
    = Edit ResourceId
    | StopEditing ResourceId
    | Update ResourceId update
    | Save ResourceId
    | SaveSucceeded ResourceId resource
    | SaveFailed ResourceId String


update : Resource r u -> Msg r u -> Model r -> ( Model r, Cmd (Msg r u) )
update resource msg model =
    case msg of
        Edit id ->
            { model | list = List.map (startEditing id) model.list } ! []

        StopEditing id ->
            { model | list = List.map (stopEditing id) model.list } ! []

        Update id update ->
            doUpdate id (resource.update update) model ! []

        Save id ->
            case id of
                NewId ->
                    startCreate resource model

                ExistingId _ ->
                    startUpdate resource id model

        SaveSucceeded id res ->
            case id of
                NewId ->
                    finishCreate res resource model

                ExistingId _ ->
                    finishUpdate res id model

        SaveFailed _ _ ->
            model ! []


startEditing : ResourceId -> Editable r -> Editable r
startEditing id editable =
    if editable.id == id then
        { editable | editor = Just editable.original }
    else
        editable


stopEditing : ResourceId -> Editable r -> Editable r
stopEditing id editable =
    if editable.id == id then
        { editable | editor = Nothing }
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


startCreate : Resource r u -> Model r -> ( Model r, Cmd (Msg r u) )
startCreate resource model =
    case model.new.editor of
        Just res ->
            model ! [ Task.attempt (saveHandler model.new.id) (resource.store.create res) ]

        Nothing ->
            model ! []


startUpdate : Resource r u -> ResourceId -> Model r -> ( Model r, Cmd (Msg r u) )
startUpdate resource id model =
    case findEditor id model.list of
        Just res ->
            model ! [ Task.attempt (saveHandler id) (resource.store.update res) ]

        Nothing ->
            model ! []


finishCreate : r -> Resource r u -> Model r -> ( Model r, Cmd (Msg r u) )
finishCreate res resource model =
    let
        newList =
            editable (ExistingId model.nextId) res
                :: model.list
                |> List.sortWith (\r1 r2 -> resource.sortWith r1.original r2.original)

        clampedList =
            case resource.store.pageSize of
                Nothing ->
                    newList

                Just size ->
                    newList |> List.take size
    in
        { model | list = clampedList, nextId = model.nextId + 1, new = editableNewResource resource.empty } ! []


finishUpdate : r -> ResourceId -> Model r -> ( Model r, Cmd (Msg r u) )
finishUpdate resource id model =
    let
        updateItem id resource editable =
            if editable.id == id then
                { editable | original = resource, editor = Nothing }
            else
                editable
    in
        { model | list = List.map (updateItem id resource) model.list } ! []


saveHandler : ResourceId -> Result String r -> Msg r u
saveHandler id result =
    case result of
        Ok res ->
            SaveSucceeded id res

        Err err ->
            SaveFailed id err


findEditor : ResourceId -> List (Editable r) -> Maybe r
findEditor id list =
    case list of
        first :: rest ->
            if first.id == id then
                first.editor
            else
                findEditor id rest

        [] ->
            Nothing



-- VIEW


view : Resource r u -> Model r -> Html (Msg r u)
view resource model =
    resource.render model



-- UTILITIES


editableNewResource : resource -> Editable resource
editableNewResource resource =
    startEditing NewId (editable NewId resource)


editable : ResourceId -> r -> Editable r
editable id resource =
    { id = id, original = resource, editor = Nothing, validation = Pass, saveError = Nothing }
