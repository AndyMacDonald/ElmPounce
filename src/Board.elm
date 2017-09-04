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
    let
        fromIdx = case board.next of
            XMove -> board.xpos
            OMove -> board.opos

    in
        svg [ viewBox "0 0 72 72", width "300px" ]
        (Array.indexedMap (renderSquare (legal board fromIdx)) board.squares |> Array.toList)

renderSquare : (Int -> Bool) -> Int -> Square -> Svg Msg
renderSquare legalTo idx square =
    let
        (xidx, yidx) = idxToXY(idx)
        xpos = toString (10 * xidx + 1)
        ypos = toString (10 * yidx + 1)
        color = case square of
            Empty -> "white"
            Blocked -> "black"
            X -> "green"
            O -> "red"
        handler = makeHandler legalTo idx square
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

-- HELPER FUNCTIONS

idxToXY : Int -> (Int, Int)
idxToXY idx =
    (idx % 7, idx // 7)

xyToIdx : Int -> Int -> Maybe Int
xyToIdx x y =
    if x < 0 || x >= 7 || y < 0 || y >= 7 then
        Nothing
    else
        Just (y * 7 + x)

legal : Model -> Int -> Int -> Bool
legal model from to =
    let
        (fx, fy) = idxToXY from
        (tx, ty) = idxToXY to
        (dx, dy) = (tx - fx, ty - fy)

    in
        if model.next == XMove && from == -1 then
            True -- first X move can go anywhere
        else if model.next == OMove && from == -1 && to == model.xpos then
            False -- first O move can't go on X
        else if from == to then
            False -- Can't move to self
        else if dx /= 0 && dy /= 0 && abs(dx) /= abs(dy) then
            False -- Must move like a queen in chess
        else
            -- No blockers between from and to
            True

makeHandler : (Int -> Bool) -> Int -> Square -> Maybe (Svg.Attribute Msg)
makeHandler legalTo to square =
    if square /= Blocked && legalTo to then
        Just (Svg.Events.onClick (Clicked to))
    else
        Nothing
