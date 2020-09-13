module Main exposing (Item, Model, Msg(..), customHtml, init, main, subscriptions, testList3, update, view)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Browser exposing (Document, document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import SelectTwo exposing (..)
import SelectTwo.Html exposing (..)
import SelectTwo.Types exposing (..)


type alias Model =
    { selectTwo : Maybe (SelectTwo Msg)
    , test3 : List (Maybe String)
    , modalVisibility : Modal.Visibility
    }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( { selectTwo = Nothing
      , test3 = []
      , modalVisibility = Modal.hidden
      }
    , Cmd.none
    )


type Msg
    = Test3 (Maybe String)
    | SelectTwo (SelectTwoMsg Msg)
    | Test3Clear (Maybe Msg)
    | ShowModal
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTwo stmsg ->
            let
                ajaxCases =
                    Just
                        (\id_ params reset ->
                            case id_ of

                                _ ->
                                    ( model
                                    , Cmd.none
                                    )
                        )
            in
            SelectTwo.update SelectTwo stmsg ajaxCases model

        Test3 s ->
            ( { model | test3 = s :: model.test3 }
            , Cmd.none
            )


        Test3Clear (Just (Test3 s)) ->
            ( { model | test3 = model.test3 |> List.filter ((/=) s) }
            , Cmd.none
            )

        Test3Clear _ ->
            ( model
            , Cmd.none
            )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }, Cmd.none)

        CloseModal ->
                ( { model | modalVisibility = Modal.hidden }, Cmd.none)


main : Program () Model Msg
main =
    document
        { init = init
        , update = update
        , view = topView
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


topView : Model -> Document Msg
topView model =
    { title = "elm-select-two"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "height" "100%"
        , style "padding" "30px"
        , style "font-size" "16px"
        , select2Close SelectTwo
        ]
        [ select2Css
        , h1 [] [ text "Examples of Elm Select2" ]
        , Grid.row []
              [ Grid.col [ Col.md12 ]
                  [ Button.button
                      [ Button.outlinePrimary
                      , Button.onClick <| ShowModal
                      ]
                      [ text "Show modal" ]
                  ]
              ]
        , viewModal model
        ]


viewModal : Model -> Html Msg
viewModal model =
    Modal.config CloseModal
        |> Modal.large
        |> Modal.h5 [] [ text "Nice modal" ]
        |> Modal.body [] [
            p
                [ class "p2" ]
                [ text "Single Group, Multi-Select, Custom Rows, Position Absolute, Close On Clear"
                , div []
                    [ select2 SelectTwo
                        { defaults = SelectTwo.defaultsFromList (model.test3 |> List.map Test3) <| testList3 Test3
                        , id_ = "test-3"
                        , list = testList3 Test3
                        , clearMsg = Just Test3Clear
                        , width = "300px"
                        , placeholder = "Select Test"
                        , disabled = False
                        , showSearch = True
                        , multiSelect = True
                        , ajax = False
                        , delay = 0
                        , noResultsMessage = Nothing
                        , closeOnClear = True
                        }
                    ]
                ]
            , select2Dropdown SelectTwo (Just customHtml) model
        ]
        |> Modal.footer []
            [ Button.button
                [ Button.secondary
                , Button.attrs [ onClick CloseModal ]
                ]
                [ text "Close" ]
            ]
        |> Modal.view model.modalVisibility



type alias Item =
    { id : Int, name : String }


testList3 : (Maybe String -> msg) -> List (GroupSelectTwoOption msg)
testList3 msg =
    [ ( Just "a", "Disable All Other Boxes", "Group 1" ), ( Just "b", "b", "Group 1" ), ( Just "c", "c", "Group 2" ) ] |> SelectTwo.basicGroupSelectOptions msg


customHtml : SelectTwoOption Msg -> Maybe (Html Msg)
customHtml ( msg, txt, sel ) =
    case msg of
        Just (Test3 _) ->
            Just <|
                span
                    [ style "width" "100%"
                    , style "text-align" "center"
                    , style "display" "inline-block"
                    ]
                    [ text txt
                    ]

        _ ->
            Nothing

