module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (src)

import Element exposing (Element, row, column, el, text)
import Element.Events exposing (onClick)
import Array


--- GRID ---

type CellValue = X | O
type alias Grid = Array.Array (Array.Array (Maybe CellValue))


emptyGrid : Grid
emptyGrid = Array.fromList
  [ Array.fromList [Nothing, Nothing, Nothing]
  , Array.fromList [Nothing, Nothing, Nothing]
  , Array.fromList [Nothing, Nothing, Nothing]
  ]


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
   column [] <| List.map2 viewRow [0, 1, 2] rows

  

---- MODEL ----

type Player = PX | PO

switchPlayer playa =
  case playa of
    PX -> PO
    PO -> PX

type alias Model =
    { grid: Grid
    , currentPlayer: Player
    }


init : ( Model, Cmd Msg )
init =
    ( {grid = emptyGrid, currentPlayer = PX}, Cmd.none )



---- UPDATE ----


type Msg
    = Click Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update (Click row col) model =
  let
      {grid, currentPlayer} = model
      val = case currentPlayer of
        PX -> X
        PO -> O
      mrow = Array.get row grid
      ngrid = case mrow of
        Nothing -> grid
        Just row_ ->
            case Array.get col row_ of
              Nothing -> grid
              Just cell ->
                case cell of
                  Nothing -> Array.set row (Array.set col (Just val) row_) grid
                  Just a -> grid
  in
      if model.grid == ngrid then
        ( model, Cmd.none )
      else
        ({grid = ngrid, currentPlayer = switchPlayer currentPlayer}, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
  Element.layout [] <|
    viewGrid <|
      Array.toList <|
        Array.map Array.toList model.grid



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
