module Resource.Render.Basic exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Resource exposing (..)


basicRenderer : FieldBuilder r u -> Renderer r u
basicRenderer fields =
    \model ->
        div [] (List.map (resourceView fields) model.list ++ [ resourceView fields model.new ])


resourceView : FieldBuilder r u -> Editable r -> Html (Msg r u)
resourceView fields editable =
    let
        view =
            case editable.editor of
                Just res ->
                    Html.form [ onSubmit Create ] (List.map (fieldInput editable) (fields res) ++ [ createButton ])

                Nothing ->
                    div [] (List.map fieldView (fields editable.original) ++ [ button [ onClick (Edit editable.id) ] [] ])
    in
        li [] [ view ]


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


fieldInput : Editable r -> Field u -> Html (Msg r u)
fieldInput { id } field =
    case field of
        TextField title content op ->
            div []
                [ label [] [ text title ]
                , input [ type_ "text", value content, onInput (op >> Update id) ] []
                ]

        TextArea title content op ->
            div []
                [ label [] [ text title ]
                , textarea [ onInput (op >> Update id) ] [ text content ]
                ]

        NumberField title content op ->
            div []
                [ label [] [ text title ]
                , input [ type_ "number", onInput (handleNumber content >> op >> Update id) ] [ text (toString content) ]
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


createButton : Html (Msg r u)
createButton =
    button [ type_ "submit" ] [ text "Create" ]
