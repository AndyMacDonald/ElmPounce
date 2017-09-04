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
type alias Model =
    { blocked : Set Int
    , next : Player
    , xpos : Int
    , opos : Int
    }

init : Model
init =
    { blocked = Set.empty
    , next = XMove
    , xpos = -1
    , opos = -1
    }

view : Model -> Html Msg
view board = 
    svg [ viewBox "0 0 72 72", width "300px" ]
        (List.map (renderSquare board) (List.range 0 48))

renderSquare : Model -> Int -> Svg Msg
renderSquare model idx =
    let
        (xidx, yidx) = idxToXY(idx)
        xpos = toString (10 * xidx + 1)
        ypos = toString (10 * yidx + 1)
        color = squareColor model idx
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
                    { blocked = insert model.xpos model.blocked
                    , next = OMove
                    , xpos = idx
                    , opos = model.opos
                    }
                OMove ->
                    { blocked = insert model.opos model.blocked
                    , next = XMove
                    , xpos = model.xpos
                    , opos = idx
                    }
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
            XMove -> model.xpos
            OMove -> model.opos
        (fx, fy) = idxToXY from
        (tx, ty) = idxToXY to
        (dx, dy) = (tx - fx, ty - fy)

    in
        if from == -1 then -- First move for each side is a special case
            (if to == model.xpos then -- can move anywhere except O not allow on top of X
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
        List.any (\x -> member x model.blocked) idxs

makeHandler : Model -> Int -> Maybe (Svg.Attribute Msg)
makeHandler model to =
    if not (Set.member to model.blocked) && legal model to then
        Just (Svg.Events.onClick (Clicked to))
    else
        Nothing

squareColor : Model -> Int -> String
squareColor model idx =
    if idx == model.xpos then
        "green"
    else if idx == model.opos then
        "red"
    else if member idx model.blocked then
        "black"
    else
        "white"
