module Resource.Render.Basic exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Resource exposing (..)


basicRenderer : FieldBuilder r u -> Renderer r u
basicRenderer fields =
    \model ->
        ul [ class "basic-resource-list" ] (List.map (resourceView fields) model.list ++ [ resourceView fields model.new ])


resourceView : FieldBuilder r u -> Editable r -> Html (Msg r u)
resourceView fields editable =
    let
        view =
            case editable.editor of
                Just res ->
                    Html.form [ onSubmit (Save editable.id) ] (List.map (fieldInput editable) (fields res) ++ formButtons editable)

                Nothing ->
                    div [] (List.map fieldView (fields editable.original) ++ [ editButton editable ])
    in
        li [ class "basic-resource" ] [ view ]


fieldView : Field u -> Html (Msg r u)
fieldView field =
    case field of
        TextField title content _ ->
            p [] [ text title, text content ]

        TextArea title content _ ->
            p [] [ text title, text content ]

        NumberField title n _ ->
            pre [] [ text title, text (toString n) ]

        CheckboxField title v _ ->
            if v then
                p [] [ text title, text "yes" ]
            else
                p [] [ text title, text "no" ]


fieldInput : Editable r -> Field u -> Html (Msg r u)
fieldInput { id } field =
    case field of
        TextField title content op ->
            div []
                [ label [] [ text title ]
                , input [ class "basic-resource-input", type_ "text", value content, onInput (op >> Update id) ] []
                ]

        TextArea title content op ->
            div []
                [ label [] [ text title ]
                , textarea [ class "basic-resource-input", onInput (op >> Update id) ] [ text content ]
                ]

        NumberField title content op ->
            div []
                [ label [] [ text title ]
                , input [ class "basic-resource-input", type_ "number", onInput (handleNumber content >> op >> Update id) ] [ text (toString content) ]
                ]

        CheckboxField title content op ->
            div []
                [ label [] [ text title ]
                , input [ type_ "checkbox", checked content, onClick (op (not content) |> Update id) ] []
                ]


handleNumber : Float -> String -> Float
handleNumber current new =
    case String.toFloat new of
        Err _ ->
            current

        Ok f ->
            f


editButton : Editable r -> Html (Msg r u)
editButton editable =
    button [ class "basic-resource-button", onClick (Edit editable.id) ] [ text "Edit" ]


formButtons : Editable r -> List (Html (Msg r u))
formButtons editable =
    case editable.id of
        NewId ->
            [ button [ class "basic-resource-button", type_ "submit" ] [ text "Create" ] ]

        ExistingId id ->
            [ button [ class "basic-resource-button", type_ "submit" ] [ text "Update" ]
            , button [ class "basic-resource-button", type_ "button", onClick (StopEditing editable.id) ] [ text "Cancel" ]
            ]
