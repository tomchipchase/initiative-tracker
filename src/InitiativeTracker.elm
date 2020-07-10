module InitiativeTracker exposing (..)

import Browser
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type Msg
    = SetName String
    | SetInitiative String
    | AddPlayer


type alias Player =
    { name : String
    , initiative : Int
    }


type alias Model =
    { playerList : List Player
    , input : Player
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


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewPlayerList model
        , viewPlayerForm model
        ]


init : Model
init =
    Model [] (Player "" 0)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
