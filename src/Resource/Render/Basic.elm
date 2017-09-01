module Resource.Render.Basic exposing (..)

import Html exposing (..)
import Resource exposing (..)


basicRenderer : FieldBuilder r u -> Renderer r u
basicRenderer fields =
    \model ->
        div [] [ listView fields model, createForm fields model ]


listView : FieldBuilder r u -> Model r -> Html (Msg r u)
listView fields model =
    case model.list of
        NotRequested ->
            text "Waiting ..."

        Loading ->
            text "Loading ..."

        Loaded items ->
            ul [] (List.map (resourceView fields) items)

        Error err ->
            p [] [ text err ]


resourceView : FieldBuilder r u -> ( r, Maybe r ) -> Html (Msg r u)
resourceView fields ( resource, editing ) =
    li [] (List.map fieldView (fields resource))


fieldView : Field u -> Html (Msg r u)
fieldView field =
    case field of
        TextField _ content _ ->
            h1 [] [ text content ]

        TextArea _ content _ ->
            p [] [ text content ]

        NumberField _ n _ ->
            pre [] [ text (toString n) ]

        CheckboxField _ v _ ->
            if v then
                text "yes"
            else
                text "no"


createForm : FieldBuilder r u -> Model r -> Html (Msg r u)
createForm _ _ =
    form [] []
