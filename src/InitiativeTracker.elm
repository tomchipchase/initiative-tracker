module InitiativeTracker exposing (..)

import Array
import Browser
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


indexOf : (a -> Bool) -> List a -> Int
indexOf match list =
    list
        |> List.indexedMap Tuple.pair
        |> List.filter (\( idx, item ) -> match item)
        |> List.map Tuple.first
        |> List.head
        |> Maybe.withDefault 0


type Msg
    = SetName String
    | SetInitiative String
    | AddPlayer
    | RemovePlayer String
    | NextPlayer
    | PreviousPlayer


type alias Player =
    { name : String
    , initiative : Int
    }


type alias Model =
    { playerList : List Player
    , input : Player
    , turn : Int
    }


type alias CurrentPlayerViewModel =
    { currentPlayer : String }


getPlayer : Int -> List Player -> String
getPlayer turn playerList =
    let
        index =
            case List.length playerList of
                0 ->
                    0

                n ->
                    modBy n turn
    in
    playerList
        |> Array.fromList
        |> Array.get index
        |> Maybe.withDefault (Player "" 0)
        |> .name


addPlayer : Model -> Model
addPlayer model =
    let
        currentPlayer =
            getPlayer model.turn model.playerList

        newPlayers =
            model.input :: model.playerList |> List.sortBy .initiative |> List.reverse

        turn =
            case model.turn of
                0 ->
                    0

                _ ->
                    newPlayers |> indexOf (\player -> player.name == currentPlayer)
    in
    { model
        | playerList = newPlayers
        , input = Player "" 0
        , turn = turn
    }


removePlayer : String -> Model -> Model
removePlayer name model =
    let
        playerCount =
            List.length model.playerList

        round =
            case playerCount of
                0 ->
                    0

                n ->
                    model.turn // n

        oldIndex =
            case playerCount of
                0 ->
                    0

                n ->
                    modBy n model.turn

        currentPlayer =
            getPlayer oldIndex model.playerList

        index =
            if currentPlayer == name then
                oldIndex + 1

            else
                oldIndex

        newPlayers =
            model.playerList |> List.filter (\player -> player.name /= name)

        newIndex =
            newPlayers |> indexOf (\player -> player.name == currentPlayer)

        turn =
            (round * (playerCount - 1)) + newIndex
    in
    { model
        | playerList = newPlayers
        , turn = turn
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
            model |> addPlayer

        RemovePlayer name ->
            model |> removePlayer name

        NextPlayer ->
            { model | turn = model.turn + 1 }

        PreviousPlayer ->
            { model | turn = model.turn - 1 }


viewPlayerList : Model -> Html.Html Msg
viewPlayerList model =
    Html.table []
        List.map
        (\player ->
            Html.tr []
                [ Html.td [] [ Html.text player.name ]
                , Html.td [] [ Html.text (String.fromInt player.initiative) ]
                , Html.td [] [ Html.button [ onClick (RemovePlayer player.name) ] [ Html.text "X" ] ]
                ]
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


viewTurnControl : Model -> Html.Html Msg
viewTurnControl model =
    Html.div []
        [ Html.button [ onClick PreviousPlayer ] [ Html.text "<" ]
        , Html.text (String.fromInt model.turn)
        , Html.button [ onClick NextPlayer ] [ Html.text ">" ]
        ]


viewCurrentPlayer : Model -> Html.Html Msg
viewCurrentPlayer model =
    let
        currentPlayer =
            getPlayer model.turn model.playerList
    in
    Html.div [] [ Html.text currentPlayer ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewCurrentPlayer model
        , viewPlayerList model
        , viewTurnControl model
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
