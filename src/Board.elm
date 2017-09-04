module Board exposing (Model, init, view, Msg(Clicked), update, Player(XMove, OMove))

import Array exposing (..)
import Dict exposing (..)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

type Player
  = XMove
  | OMove

type Square
  = Empty
  | X
  | O
  | Blocked

type Msg = 
    Clicked Int

-- The board
-- This first implementation is very squishy with too many degrees of 
-- freedom. Tighten up later.
type alias Model =
    { squares : Array Square
    , next : Player
    , xpos : Int
    , opos : Int
    }

init : Model
init =
    { squares = Array.repeat 49 Empty
    , next = XMove
    , xpos = -1
    , opos = -1
    }

view : Model -> Html Msg
view board = 
    svg [ viewBox "0 0 72 72", width "300px" ]
      (Array.indexedMap renderSquares board.squares |> Array.toList)

renderSquares : Int -> Square -> Svg Msg
renderSquares idx square =
    let
        xidx = idx % 7
        yidx = idx // 7
        xpos = toString (10 * xidx + 1)
        ypos = toString (10 * yidx + 1)
        color = case square of
            Empty -> "white"
            Blocked -> "black"
            X -> "green"
            O -> "red"
        handler = case square of
            Empty -> Just (Svg.Events.onClick (Clicked idx))
            Blocked -> Nothing
            X -> Just (Svg.Events.onClick (Clicked idx))
            O -> Just (Svg.Events.onClick (Clicked idx))  
        attributes = [x xpos, y ypos, width "10", height "10", stroke "black", strokeWidth "1", fill color]
            
    in
        case handler of
        Just callback ->
            rect (callback :: attributes) []
        Nothing ->
            rect attributes []

update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked idx ->
            case model.next of
                XMove ->
                    { squares = Array.set model.xpos Blocked (Array.set idx X model.squares)
                    , next = OMove
                    , xpos = idx
                    , opos = model.opos
                    }
                OMove ->
                    { squares = Array.set model.opos Blocked (Array.set idx O model.squares)
                    , next = XMove
                    , xpos = model.xpos
                    , opos = idx
                    }
    