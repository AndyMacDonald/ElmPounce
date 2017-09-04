module Board exposing (..)

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
type alias Board =
    { squares : Array Square
    , next : Player
    }

initialBoard : Board
initialBoard =
    { squares = Array.repeat 49 Empty
    , next = X
    }

render : Board -> Html.Html msg
render board = 
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
            