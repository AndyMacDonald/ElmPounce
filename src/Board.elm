module Board exposing (Model, init, view)

import Array exposing (..)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type Player
  = X
  | O

type Square
  = Empty
  | Blocked

-- The board
-- This first implementation is very squishy with too many degrees of 
-- freedom. Tighten up later.
type alias Model =
    { squares : Array Square
    , next : Player
    }

init : Model
init =
    { squares = Array.repeat 49 Empty
    , next = X
    }

view : Model -> Html msg
view board = 
    svg [ viewBox "0 0 72 72", width "300px" ]
      (Array.indexedMap renderSquares board.squares |> toList)

renderSquares : Int -> Square -> Svg msg
renderSquares idx square =
    let
        xidx = idx % 7
        yidx = idx // 7
        xpos = toString (10 * xidx + 1)
        ypos = toString (10 * yidx + 1)
        color = case square of
            Empty -> "white"
            Blocked -> "black"
            
    in
        rect [x xpos, y ypos, width "10", height "10", stroke "black", strokeWidth "1", fill color] []
            