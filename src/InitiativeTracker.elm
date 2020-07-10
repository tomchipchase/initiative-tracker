module InitiativeTracker exposing (..)

import Array
import Browser
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type Msg
    = SetName String
    | SetInitiative String
    | AddPlayer
    | NextPlayer


type alias Player =
    { name : String
    , initiative : Int
    }


type alias Model =
    { playerList : List Player
    , input : Player
    , turn : Int
    }


update : Msg -> Model -> Model
update msg model =
    let
        setInputName name input =
            { input | name = name }

        setInputInitiative initiative input =
            { input
                | initiative = initiative |> String.toInt |> Maybe.withDefault 0
            }
    in
    case
        msg
    of
        SetName name ->
            { model | input = model.input |> setInputName name }

        SetInitiative initiative ->
            { model | input = model.input |> setInputInitiative initiative }

        AddPlayer ->
            { model
                | playerList = model.input :: model.playerList |> List.sortBy .initiative |> List.reverse
                , input = Player "" 0
            }

        NextPlayer ->
            { model | turn = model.turn + 1 }


viewPlayerList : Model -> Html.Html Msg
viewPlayerList model =
    Html.table []
        (List.map
            (\player ->
                Html.tr []
                    [ Html.td [] [ Html.text player.name ]
                    , Html.td [] [ Html.text (String.fromInt player.initiative) ]
                    ]
            )
            model.playerList
        )


viewPlayerForm : Model -> Html.Html Msg
viewPlayerForm model =
    Html.div []
        [ Html.input
            [ placeholder "Player Name"
            , onInput SetName
            , value model.input.name
            ]
            []
        , Html.input
            [ placeholder "Initiative"
            , onInput SetInitiative
            , value (model.input.initiative |> String.fromInt)
            ]
            []
        , Html.button [ onClick AddPlayer ] [ Html.text "Add" ]
        ]


viewNextButton : Model -> Html.Html Msg
viewNextButton model =
    Html.div []
        [ Html.text (String.fromInt model.turn)
        , Html.button [ onClick NextPlayer ] [ Html.text ">" ]
        ]


viewCurrentPlayer : Model -> Html.Html Msg
viewCurrentPlayer model =
    let
        index =
            case List.length model.playerList of
                0 ->
                    0

                n ->
                    modBy n model.turn

        name =
            model.playerList
                |> Array.fromList
                |> Array.get index
                |> Maybe.withDefault (Player "" 0)
                |> .name
    in
    Html.div [] [ Html.text name ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewCurrentPlayer model
        , viewPlayerList model
        , viewNextButton model
        , viewPlayerForm model
        ]


init : Model
init =
    Model [] (Player "" 0) 0


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
