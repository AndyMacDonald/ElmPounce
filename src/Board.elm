module Board
    exposing
        ( Model
        , init
        , view
        , Msg(..)
        , update
        , statusText
        , Player(..)
        )

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



-- The board
-- This first implementation is very squishy with too many degrees of
-- freedom. Tighten up later.


type alias Model =
    { blocked : Set Int
    , xpos : Int
    , opos : Int
    , moves : Set Int
    , next : Player
    }


init : Model
init =
    { blocked = Set.empty
    , xpos = -1
    , opos = -1
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
        ( xidx, yidx ) =
            idxToXY (idx)

        xpos =
            String.fromInt (10 * xidx + 1)

        ypos =
            String.fromInt (10 * yidx + 1)

        color =
            squareColor model idx

        handler =
            makeHandler model idx

        attributes =
            [ x xpos, y ypos, width "10", height "10", stroke "black", strokeWidth "1", fill color ]
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
                        newNext =
                            OMove

                        from =
                            model.xpos

                        newModel =
                            { blocked = insert from model.blocked
                            , xpos = idx
                            , opos = model.opos
                            , moves = empty
                            , next = newNext
                            }

                        myMoves =
                            Set.filter (legal newModel newModel.xpos)
                                (Set.diff (Set.fromList allSquares) newModel.blocked)

                        newMoves =
                            Set.filter (legal newModel newModel.opos)
                                (Set.diff (Set.fromList allSquares) newModel.blocked)

                        realNext =
                            if idx == model.opos then
                                XPounced
                            else if isEmpty newMoves then
                                OBlocked
                            else if isEmpty myMoves then
                                XBlocked
                            else
                                OMove
                    in
                        { newModel | next = realNext, moves = newMoves }

                OMove ->
                    let
                        newNext =
                            XMove

                        from =
                            model.opos

                        newModel =
                            { blocked = insert from model.blocked
                            , xpos = model.xpos
                            , opos = idx
                            , moves = empty
                            , next = newNext
                            }

                        myMoves =
                            Set.filter (legal newModel newModel.opos)
                                (Set.diff (Set.fromList allSquares) newModel.blocked)

                        newMoves =
                            Set.filter (legal newModel newModel.xpos)
                                (Set.diff (Set.fromList allSquares) newModel.blocked)

                        realNext =
                            if idx == model.xpos then
                                OPounced
                            else if isEmpty newMoves then
                                XBlocked
                            else if isEmpty myMoves then
                                OBlocked
                            else
                                XMove
                    in
                        { newModel | next = realNext, moves = newMoves }

                _ ->
                    model


statusText : Model -> String
statusText model =
    case model.next of
        XMove ->
            "Blue to move"

        OMove ->
            "Red to move"

        XBlocked ->
            "Blue is trapped. Red wins!"

        OBlocked ->
            "Red is trapped. Blue wins!"

        XPounced ->
            "Blue pounced on red. Blue wins!"

        OPounced ->
            "Red pounced on blue. Red wins!"



-- HELPER FUNCTIONS
-- UPDATE HELPERS


legal : Model -> Int -> Int -> Bool
legal model from to =
    let
        ( fx, fy ) =
            idxToXY from

        ( tx, ty ) =
            idxToXY to

        ( dx, dy ) =
            ( tx - fx, ty - fy )
    in
        if from == -1 then
            -- First move for each side is a special case
            (if to == model.xpos then
                -- can move anywhere except O not allow on top of X
                False
             else
                True
            )
        else if from == to then
            False
            -- Can't move to self
        else if dx /= 0 && dy /= 0 && abs (dx) /= abs (dy) then
            False
            -- Must move like a queen in chess
        else
            -- No blockers between from and to
            not (blocked model from to (Basics.max (abs dx) (abs dy)))


blocked : Model -> Int -> Int -> Int -> Bool
blocked model from to count =
    let
        delta =
            to - from

        dstep =
            delta // count

        -- should be integral
        idxs =
            List.map (\x -> from + x * dstep) (List.range 1 count)
    in
        List.any (\x -> member x model.blocked) idxs



-- VIEW HELPERS


makeHandler : Model -> Int -> Maybe (Svg.Attribute Msg)
makeHandler model to =
    if Set.member to model.moves then
        Just (Svg.Events.onClick (Clicked to))
    else
        Nothing


squareColor : Model -> Int -> String
squareColor model idx =
    if idx == model.xpos then
        xOrPouncedColor model.next model.xpos model.opos
    else if idx == model.opos then
        "red"
    else if member idx model.blocked then
        "#222222"
    else
        "white"


xOrPouncedColor : Player -> Int -> Int -> String
xOrPouncedColor player xpos opos =
    if xpos == opos then
        if player == XPounced then
            "#0000DD"
        else
            "red"
    else
        "#0000DD"



-- GENERAL HELPERS


allSquares : List Int
allSquares =
    List.range 0 48


idxToXY : Int -> ( Int, Int )
idxToXY idx =
    ( modBy 7 idx, idx // 7 )
