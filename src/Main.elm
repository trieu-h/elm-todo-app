module Main exposing (main)

import Browser
import Css exposing (height, marginRight, paddingLeft, paddingRight, px, rem, width)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, disabled, type_, value)
import Html.Styled.Events exposing (onClick, onInput)


-- TODO: refactor select and change styling

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
    { text : String
    , state : State
    , id : Int
    , isShow : Bool
    }


type State
    = Init
    | Completed


init : Model
init =
    Model "" [] True



-- VIEW


view : Model -> Html Msg
view model =
    let
        initText =
            Init |> stateToText

        completedText =
            Completed |> stateToText

        options =
            [ option [ value "All" ] [ text "All" ]
            , option [ value initText ] [ text initText ]
            , option [ value completedText ] [ text completedText ]
            ]
    in
    div [ class "container" ]
        [ div [ class "mx-auto" ]
            [ h1 [ class "text-center" ] [ text "TODO APP" ]
            , div [ class "d-flex flex-row align-items-center justify-content-center mb-3" ]
                [ input
                    [ type_ "text"
                    , css [ marginRight (px 5) ]
                    , value model.inputText
                    , onInput InputChange
                    , class "form-control"
                    ]
                    []
                , button
                    [ type_ "button"
                    , css [ marginRight (px 5) ]
                    , class "btn btn-primary"
                    , onClick Add
                    , disabled model.isButtonDisabled
                    ]
                    [ text "Add" ]
                , select [ class "form-select", onInput Filter ] options
                ]
            , div [] (todosView model.todos)
            ]
        ]


todosView : List Todo -> List (Html Msg)
todosView todos =
    let
        todoToView =
            \i todo ->
                let
                    extraClass =
                        case todo.state of
                            Completed ->
                                " border border-3 border-success"

                            Init ->
                                ""
                in
                if todo.isShow == True then
                    div [ class "input-group mb-3 d-flex align-items-center justify-content-center" ]
                        [ input
                            [ type_ "text"
                            , class ("form-control" ++ extraClass)
                            , value todo.text
                            ]
                            []
                        , div [ class "input-group-append" ]
                            [ button
                                [ type_ "button"
                                , class "btn btn-success"
                                , onClick (Complete i)
                                ]
                                [ text "Complete" ]
                            , button
                                [ type_ "button"
                                , class "btn btn-danger"
                                , onClick (Delete todo.id)
                                ]
                                [ text "Delete" ]
                            ]
                        ]

                else
                    div [] []
    in
    List.indexedMap todoToView todos



-- UPDATE


type Msg
    = InputChange String
    | Add
    | Delete Int
    | Complete Int
    | Filter String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChange inputText ->
            { model
                | inputText = inputText
                , isButtonDisabled = isStringEmpty inputText
            }

        Add ->
            let
                item =
                    { text = model.inputText, state = Init, id = List.length model.todos, isShow = True } |> List.singleton

                newTodos =
                    List.append model.todos item
            in
            { model
                | todos = newTodos
                , inputText = ""
                , isButtonDisabled = True
            }

        Delete id ->
            { model | todos = List.filter (\x -> x.id /= id) model.todos }

        Complete id ->
            let
                updateTodo =
                    \todo ->
                        if todo.id == id then
                            { todo | state = Completed }

                        else
                            todo
            in
            { model | todos = List.map updateTodo model.todos }

        Filter mode ->
            let
                filterAll =
                    \todo -> { todo | isShow = True }

                filterInit =
                    \todo ->
                        if todo.state == Init then
                            { todo | isShow = True }

                        else
                            { todo | isShow = False }

                filterCompleted =
                    \todo ->
                        if todo.state == Completed then
                            { todo | isShow = True }

                        else
                            { todo | isShow = False }
            in
            case mode of
                "All" ->
                    { model | todos = List.map filterAll model.todos }

                "Init" ->
                    { model | todos = List.map filterInit model.todos }

                "Completed" ->
                    { model | todos = List.map filterCompleted model.todos }

                _ ->
                    model



-- UTIL


isStringEmpty : String -> Bool
isStringEmpty string =
    string |> String.trim |> String.isEmpty


stateToText : State -> String
stateToText state =
    case state of
        Init ->
            "Init"

        Completed ->
            "Completed"


textToState : String -> State
textToState string =
    if string == "Init" then
        Init

    else
        Completed
