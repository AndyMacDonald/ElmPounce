module Board exposing (Model, init, view, Msg(Clicked, Reset), update, Player(XMove, OMove))

import Set exposing (..)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

type Player
  = XMove
  | OMove

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
            let
                squares = model.squares
                newNext = case model.next of
                    XMove -> OMove
                    OMove -> XMove
                newSquares = case model.next of
                    XMove -> 
                            { blocked = insert squares.xpos squares.blocked
                            , xpos = idx
                            , opos = squares.opos
                            }
                    OMove ->
                            { blocked = insert squares.opos squares.blocked
                            , xpos = squares.xpos
                            , opos = idx
                            }
                newModel =  { squares = newSquares
                            , moves = Set.fromList allSquares
                            , next = newNext
                            }
            in
                { newModel | moves = Set.filter (legal newModel) newModel.moves }
        Reset -> model

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

legal : Model -> Int -> Bool
legal model to =
    let
        from = case model.next of
            XMove -> model.squares.xpos
            OMove -> model.squares.opos
        (fx, fy) = idxToXY from
        (tx, ty) = idxToXY to
        (dx, dy) = (tx - fx, ty - fy)

    in
        if from == -1 then -- First move for each side is a special case
            (if to == model.squares.xpos then -- can move anywhere except O not allow on top of X
                False 
             else
                True)
        else if from == to then
            False -- Can't move to self
        else if dx /= 0 && dy /= 0 && abs(dx) /= abs(dy) then
            False -- Must move like a queen in chess
        else
            -- No blockers between from and to
            not (blocked model from to (Basics.max (abs dx) (abs dy)))

blocked : Model -> Int -> Int -> Int -> Bool
blocked model from to count =
    let
        delta = to - from
        dstep = delta // count -- should be integral
        idxs = List.map (\x -> from + x * dstep) (List.range 1 count)

    in
        List.any (\x -> member x model.squares.blocked) idxs

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

allSquares : List Int
allSquares =
    List.range 0 48
