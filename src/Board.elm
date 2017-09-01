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
    { squares : Array (Array Square)
    , next : Player
    }

initialBoard : Board
initialBoard =
    { squares = Array.repeat 7 (Array.repeat 7 Empty)
    , next = X
    }

render : Board -> Html.Html msg
render board = 
    svg [ viewBox "0 0 72 72", width "300px" ]
      (Array.indexedMap renderRow board.squares |> foldl List.append [])

renderRow : Int -> Array Square -> List (Svg msg)
renderRow idx row =
    (Array.indexedMap (renderColumn idx) row |> Array.toList)

renderColumn : Int -> Int -> Square -> Svg msg
renderColumn yidx xidx square =
    let
        xpos = toString (10 * xidx + 1)
        ypos = toString (10 * yidx + 1)
        color = case square of
            Empty -> "white"
            Blocked -> "black"
            
    in
        rect [x xpos, y ypos, width "10", height "10", stroke "black", strokeWidth "1", fill color] []
            