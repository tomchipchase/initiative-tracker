module InitiativeTracker exposing (..)

import Browser
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type Msg
    = SetName String
    | SetInitiative String
    | AddPlayer


type alias Model =
    { playerName : String
    , playerInitiative : Int
    }


update : Msg -> Model -> Model
update msg model =
    case
        msg
    of
        SetName name ->
            { model | playerName = name }

        SetInitiative initiative ->
            { model
                | playerInitiative = initiative |> String.toInt |> Maybe.withDefault 0
            }

        AddPlayer ->
            init


viewPlayerForm : Model -> Html.Html Msg
viewPlayerForm model =
    Html.form []
        [ Html.input
            [ placeholder "Player Name"
            , onInput SetName
            ]
            []
        , Html.input
            [ placeholder "Initiative"
            , onInput SetInitiative
            ]
            []
        , Html.button
            [ onClick AddPlayer
            ]
            [ Html.text "Add" ]
        ]


init : Model
init =
    Model "" 0


main =
    Browser.sandbox
        { init = Model "" 0
        , update = update
        , view = viewPlayerForm
        }
