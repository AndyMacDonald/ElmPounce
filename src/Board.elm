module Board exposing (..)

import Array exposing (..)

type Player
  = X
  | O

-- The board
-- This first implementation is very squishy with too many degrees of 
-- freedom. Tighten up later.
type Board =
    Board
        { blocked : Array Bool
        , xpos : Int
        , opos : Int
        , turn : Player
        }

-- initialBoard : () -> Board
initialBoard =
    Board
        { blocked = Array.repeat 49 False
        , xpos = -1
        , opos = -1
        , turn = X
        }
