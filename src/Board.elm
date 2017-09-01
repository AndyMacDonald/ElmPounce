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
type Board =
    Board
        { squares : Array (Array Square)
        , next : Player
        }

initialBoard : Board
initialBoard =
    Board
        { squares = Array.repeat 7 (Array.repeat 7 Empty)
        , next = X
        }

render : Board -> Html.Html msg
render board = 
    svg [ viewBox "0 0 70 70", width "300px" ]
      [ circle [ cx "35", cy "35", r "30", fill "#0B79CE" ] []
      ]