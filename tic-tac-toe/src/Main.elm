module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (src)

import Element exposing (..)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Background as Background
import Array

import Grid
import ListExtra



---- MODEL ----

type Player = PX | PO

type CellValue = X | O

type GameState = Playing Player | Finish Player


type alias Board =
    Grid.Grid (Maybe CellValue)


emptyBoard : Board
emptyBoard =
    Grid.square 3 Nothing


type alias Model =
    { board : Board
    , gameState: GameState
    }


init : ( Model, Cmd Msg )
init =
    ( {board = emptyBoard, gameState = Playing PX}
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Set (Int, Int)


winingForValue : Board -> CellValue -> Bool
winingForValue board val =
    let
        allConsiderations =
            [ Grid.mainDiagonal board
            , Grid.secondaryDiagonal board
            ]
            ++ Grid.allRows board
            ++ Grid.allColumns board

        allSameForConsideration = ListExtra.allTrue << List.map (\elm -> elm == Just val)
    in
        ListExtra.anyTrue <| List.map allSameForConsideration allConsiderations


update : Msg -> Model -> ( Model, Cmd Msg )
update (Set (x, y)) model =
    case model.gameState of
        Finish currentPlayer ->
            (model, Cmd.none)

        Playing currentPlayer ->
            let
                {board} = model
                cellVal =
                    case currentPlayer of
                        PX -> X
                        PO -> O

                updatedBoard = Grid.set (x, y) (Just cellVal) board

                switchedPlayer =
                    case currentPlayer of
                        PX -> PO
                        PO -> PX
            in
                if model.board == updatedBoard then
                    (model, Cmd.none)
                else if winingForValue updatedBoard cellVal then
                    ({board = updatedBoard, gameState = Finish currentPlayer}, Cmd.none)
                else
                    ({board = updatedBoard, gameState = Playing switchedPlayer}, Cmd.none)




-- VIEW

viewPlayer : Player -> String
viewPlayer playa =
    case playa of
        PX -> "X"
        PO -> "O"


viewBoard : Board -> Element Msg
viewBoard board =
    let
        cellSize = 100
        gap = 10
        gridSize = cellSize * 3 + 4 * gap

        viewValue xo =
            case xo of
                X -> "X"
                O -> "O"

        cellStyles =
            [ width (px cellSize)
            , height (px cellSize)
            , centerX
            , centerY
            , Font.color (rgba255 119 110 101 1)
            , Font.size cellSize
            ]

        viewCell (x, y) val =
            case val of
                Nothing ->
                    el (cellStyles ++ [Background.color (rgba255 238 228 218 0.35), onClick (Set (x, y))])
                        (text "")

                Just a ->
                    el (cellStyles ++ [Background.color (rgba255 238 228 218 1)]) (text <| viewValue a)

        viewRow y cells =
            row [spacing gap] <| List.map2 viewCell [(0, y), (1, y), (2, y)] cells
    in
        el [width (px gridSize), height (px gridSize), Background.color (rgb255 187 173 160)]
            <| column [padding gap, spacing gap]
                <| List.map2 viewRow [0, 1, 2]
                    <| Grid.toList board


---- VIEW ----



view : Model -> Html Msg
view {board, gameState} =
    let
        viewCurrentPlayer currentPlayer = 
            el [Font.color (rgba255 119 110 101 1), Font.size 100] (text <| "Turn: " ++ viewPlayer currentPlayer)

        viewWonMsg currentPlayer =
            el [Font.color (rgba255 119 110 101 1), Font.size 100] (text <| "Won:  " ++ viewPlayer currentPlayer)

        topBanner =
            case gameState of
                Playing currentPlayer ->
                    viewCurrentPlayer currentPlayer

                Finish currentPlayer ->
                    viewWonMsg currentPlayer
    in
        Element.layout [] <|
            column []
                [ topBanner
                , viewBoard board
                ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
