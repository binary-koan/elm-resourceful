module Resource.Render.Basic exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
createForm fields model =
    let
        newFields =
            fields model.new
    in
        Html.form [] (List.map (fieldInput model.new) newFields ++ [ createButton ])


fieldInput : r -> Field u -> Html (Msg r u)
fieldInput resource field =
    case field of
        TextField title content op ->
            div []
                [ label [] [ text title ]
                , input [ type_ "text", value content, onInput (op >> Update resource) ] []
                ]

        TextArea title content op ->
            div []
                [ label [] [ text title ]
                , textarea [ onInput (op >> Update resource) ] [ text content ]
                ]

        NumberField title content op ->
            div []
                [ label [] [ text title ]
                , input [ type_ "number", onInput (handleNumber content >> op >> Update resource) ] [ text (toString content) ]
                ]

        CheckboxField title content op ->
            div []
                [ label [] [ text title ]
                , input [ type_ "checkbox", checked content, onClick (op (not content) |> Update resource) ] []
                ]


handleNumber : Float -> String -> Float
handleNumber current new =
    case String.toFloat new of
        Err _ ->
            current

        Ok f ->
            f


createButton : Html (Msg r u)
createButton =
    button [ type_ "submit", onClick Create ] [ text "Create" ]
