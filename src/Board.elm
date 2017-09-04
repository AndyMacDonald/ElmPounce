module Board exposing (Model, init, view, Msg(Clicked, Reset), update, statusText)

import Set exposing (..)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

type Player
  = XMove
  | OMove
  | XBlocked
  | OBlocked
  | XPounced
  | OPounced

type Msg
    = Clicked Int
    | Reset

-- The board
-- This first implementation is very squishy with too many degrees of 
-- freedom. Tighten up later.
type alias Squares =
    { blocked : Set Int
    , xpos : Int
    , opos : Int
    }
type alias Model =
    { squares : Squares
    , moves : Set Int
    , next : Player
    }

init : Model
init =
    { squares = { blocked = Set.empty, xpos = -1, opos = -1 }
    , moves = Set.fromList allSquares
    , next = XMove
    }

view : Model -> Html Msg
view model = 
    svg [ viewBox "0 0 72 72", width "300px" ]
        (List.map (renderSquare model) allSquares)

renderSquare : Model -> Int -> Svg Msg
renderSquare model idx =
    let
        (xidx, yidx) = idxToXY(idx)
        xpos = toString (10 * xidx + 1)
        ypos = toString (10 * yidx + 1)
        color = squareColor model.squares idx
        handler = makeHandler model idx
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
                    let
                        squares = model.squares
                        newNext = OMove
                        from = squares.xpos
                        newSquares = 
                             { blocked = insert from squares.blocked
                            , xpos = idx
                            , opos = squares.opos
                            }
                        newMoves = Set.filter (legal newSquares from) (Set.fromList allSquares)
                        realNext =
                            if idx == squares.opos then
                                XPounced
                            else if isEmpty newMoves then
                                XBlocked
                            else
                                OMove
                    in
                        { squares = newSquares
                        , next = realNext
                        , moves = newMoves
                        }
                            
                OMove -> 
                     let
                        squares = model.squares
                        newNext = XMove
                        from = squares.opos
                        newSquares = 
                             { blocked = insert from squares.blocked
                            , xpos = squares.xpos
                            , opos = idx
                            }
                        newMoves = Set.filter (legal newSquares from) (Set.fromList allSquares)
                        realNext =
                            if idx == squares.xpos then
                                OPounced
                            else if isEmpty newMoves then
                                OBlocked
                            else
                                XMove
                    in
                        { squares = newSquares
                        , next = realNext
                        , moves = newMoves
                        }
                _ -> model

        Reset -> model

statusText : Model -> String
statusText model =
    case model.next of
        XMove -> "Green to move"
        OMove -> "Red to move"
        XBlocked -> "Green is blocked. Red wins!"
        OBlocked -> "Red is blocked. Green wins!"
        XPounced -> "Green pounced on red. Green wins!"
        OPounced -> "Red pounced on green. Red wins!"

-- HELPER FUNCTIONS

-- UPDATE HELPERS

legal : Squares -> Int -> Int -> Bool
legal squares from to =
    let
        (fx, fy) = idxToXY from
        (tx, ty) = idxToXY to
        (dx, dy) = (tx - fx, ty - fy)

    in
        if from == -1 then -- First move for each side is a special case
            (if to == squares.xpos then -- can move anywhere except O not allow on top of X
                False 
             else
                True)
        else if from == to then
            False -- Can't move to self
        else if dx /= 0 && dy /= 0 && abs(dx) /= abs(dy) then
            False -- Must move like a queen in chess
        else
            -- No blockers between from and to
            not (blocked squares from to (Basics.max (abs dx) (abs dy)))

blocked : Squares -> Int -> Int -> Int -> Bool
blocked squares from to count =
    let
        delta = to - from
        dstep = delta // count -- should be integral
        idxs = List.map (\x -> from + x * dstep) (List.range 1 count)

    in
        List.any (\x -> member x squares.blocked) idxs

-- VIEW HELPERS
makeHandler : Model -> Int -> Maybe (Svg.Attribute Msg)
makeHandler model to =
    if Set.member to model.moves then
        Just (Svg.Events.onClick (Clicked to))
    else
        Nothing

squareColor : Squares -> Int -> String
squareColor squares idx =
    if idx == squares.xpos then
        "green"
    else if idx == squares.opos then
        "red"
    else if member idx squares.blocked then
        "black"
    else
        "white"

-- GENERAL HELPERS

allSquares : List Int
allSquares =
    List.range 0 48

idxToXY : Int -> (Int, Int)
idxToXY idx =
    (idx % 7, idx // 7)
