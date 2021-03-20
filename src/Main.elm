module Main exposing (main)

import Browser
import Css exposing (width, height, rem, px, marginRight)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, disabled, type_, value)
import Html.Styled.Events exposing (onClick, onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }



-- MODEL


type alias Model =
    { inputText : String
    , todos : List Todo
    , isButtonDisabled : Bool
    }


type alias Todo =
    String


init : Model
init =
    Model "" [] True



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "text-center" ]
            [ h1 [] [ text "TODO APP" ]
            , div [ class "d-flex flex-row align-items-center justify-content-center mb-3" ]
                [ input
                    [ type_ "text"
                    , css [ marginRight (px 3), height (px 38) ]
                    , value model.inputText
                    , onInput InputChange
                    ]
                    []
                , button
                    [ type_ "button"
                    , class "btn btn-primary"
                    , onClick Save
                    , disabled model.isButtonDisabled
                    ]
                    [ text "Save" ]
                ]
            ]
        , div [ class "mx-auto", css [ width (rem 30) ] ] (todosView model.todos)
        ]


todosView : List Todo -> List (Html Msg)
todosView todos =
    let
        todoToView =
            \i todo ->
                div [ class "input-group mb-3" ]
                    [ input [ type_ "text", class "form-control", value todo ] []
                    , div [ class "input-group-append" ]
                        [ button [ type_ "button", class "btn btn-success" ] [ text "Complete" ]
                        , button [ type_ "button", class "btn btn-danger", onClick (Delete i) ] [ text "Delete" ]
                        ]
                    ]
    in
    List.indexedMap todoToView todos


isStringEmpty : String -> Bool
isStringEmpty string =
    string |> String.trim |> String.isEmpty 



-- UPDATE


type Msg
    = InputChange String
    | Save
    | Delete Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChange inputText ->
            { model
                | inputText = inputText
                , isButtonDisabled = isStringEmpty inputText 
            }

        Save ->
            { model | todos = List.append model.todos [ model.inputText ]
                    , inputText = ""
                    , isButtonDisabled = True
            }

        Delete index ->
            { model | todos = removeFromList index model.todos }



-- UTIL


removeFromList : Int -> List a -> List a
removeFromList i xs =
    List.take i xs ++ List.drop (i + 1) xs
