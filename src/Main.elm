module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (rows, style)
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { cells : List CellState, playerTurn : Player, message : String }


type CellState
    = Blank
    | SelectedByX
    | SelectedByO


type Player
    = Player1
    | Player2


type GameState
    = GameWon
    | GameDraw
    | GameActive


init : Model
init =
    { cells = List.repeat 9 Blank, playerTurn = Player1, message = "" }



-- UPDATE


type Msg
    = CellClicked Int
    | ResetClicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        CellClicked cellNum ->
            clickCell cellNum model

        ResetClicked ->
            init


clickCell : Int -> Model -> Model
clickCell cellNum model =
    let
        togglePlayer =
            case model.playerTurn of
                Player1 ->
                    Player2

                Player2 ->
                    Player1

        newCellState =
            case model.playerTurn of
                Player1 ->
                    SelectedByX

                Player2 ->
                    SelectedByO

        endGameText =
            case model.playerTurn of
                Player1 ->
                    "Player 2 wins!"

                Player2 ->
                    "Player 1 wins!"
    in
    case ( List.getAt cellNum model.cells, gameState model.cells ) of
        ( Just Blank, GameActive ) ->
            { model | cells = List.setAt cellNum newCellState model.cells, playerTurn = togglePlayer }

        _ ->
            { model | message = endGameText }


gameState : List CellState -> GameState
gameState cellStates =
    let
        allLines =
            [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ], [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ], [ 0, 4, 8 ], [ 2, 4, 6 ] ]

        indicesToStates : List Int -> List CellState
        indicesToStates indices =
            List.map (\x -> Maybe.withDefault Blank (List.getAt x cellStates)) indices

        getLineElement : List CellState -> CellState
        getLineElement line =
            Maybe.withDefault Blank (List.getAt 0 line)

        checkLine : List CellState -> Bool
        checkLine line =
            List.all (\x -> x == getLineElement line && x /= Blank) line
    in
    if List.any (\x -> checkLine (indicesToStates x)) allLines == True then
        GameWon

    else if List.all (\x -> x /= Blank) cellStates == True then
        GameDraw

    else
        GameActive



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ row 0 model
        , row 1 model
        , row 2 model
        , button [ onClick ResetClicked ] [ text "Reset" ]
        , text model.message
        ]


row : Int -> Model -> Html Msg
row rowNum model =
    div [ style "display" "flex" ]
        [ cell (rowNum * 3) model
        , cell (rowNum * 3 + 1) model
        , cell (rowNum * 3 + 2) model
        ]


cell : Int -> Model -> Html Msg
cell cellNum model =
    let
        cellText =
            case List.getAt cellNum model.cells of
                Just SelectedByO ->
                    "O"

                Just SelectedByX ->
                    "X"

                _ ->
                    ""
    in
    div []
        [ button [ onClick (CellClicked cellNum) ] [ text cellText ] ]
