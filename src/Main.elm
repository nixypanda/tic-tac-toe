module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (src)

import Element exposing (..)
import Element.Events exposing (onClick)
import Array

import Grid


--- GRID ---

type CellValue = X | O
type alias Board = Grid.Grid (Maybe CellValue)


emptyGrid : Board
emptyGrid = Grid.square 3 Nothing


viewRow r cells =
  let
      reprCell xo =
        case xo of
          X -> "X"
          O -> "O"

      cell c val =
        case val of
          Nothing -> el [onClick (Click r c)] (text "*")
          Just a -> el [] (text <| reprCell a)
  in
      row [] <| List.map2 cell [0, 1, 2] cells


viewGrid rows =
    el [width (px 500), height (px 600)]
        <| column [padding 10, spacing 10]
            <| List.map2 viewRow [0, 1, 2] rows

  

---- MODEL ----

type Player = PX | PO

switchPlayer playa =
  case playa of
    PX -> PO
    PO -> PX

type alias Model =
    { board : Board
    , currentPlayer: Player
    }


init : ( Model, Cmd Msg )
init =
    ({board = emptyGrid, currentPlayer = PX}, Cmd.none)



---- UPDATE ----


type Msg
    = Click Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update (Click row col) model =
  let
      {board, currentPlayer} = model
      val = case currentPlayer of
        PX -> X
        PO -> O

      updatedBoard = Grid.set (col, row) (Just val) model.board
      updatedModel = {board = updatedBoard, currentPlayer = switchPlayer currentPlayer} 
  in
      if model.board == updatedBoard then
        (model, Cmd.none)
      else
        (updatedModel, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
  Element.layout [] <|
    viewGrid <| Grid.toList model.board



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
