module Baba.Make exposing ( doTransformations )

import Baba.Cell as Cell exposing ( Grid )
import Baba.Rules exposing ( Rule(..), getApplicableTransform )

import Baba.LinkedGrid as LinkedGrid

-------------------------------
-- "makes" and transformations

emptyGrid : Grid
emptyGrid = LinkedGrid.make Cell.emptyCell 0 0


doTransformations : ( List Rule, List Rule ) -> Grid -> Grid
doTransformations ( pos, neg ) grid =
    let
        objFold : ( Int, Int, Cell.Object ) -> Cell.Grid -> Cell.Grid
        objFold ( x, y, object ) objFoldGrid =
            let
                updateCell : Cell.ObjectKind -> Cell.Location -> Cell.Location
                updateCell kind location =
                    let
                        id = Cell.getObjectId object
                        withoutObject = 
                            (LinkedGrid.getContents location).contents
                            |> List.filter (Cell.getObjectId >> (/=) id)

                        updatedObject = Cell.setObjectWordAndFlags kind 0 object
                    in
                        LinkedGrid.setContents (Cell.makeCell (updatedObject :: withoutObject)) location

                ruleFold : Rule -> Cell.Grid -> Cell.Grid
                ruleFold rule ruleApplyGrid =
                    case LinkedGrid.at x y ruleApplyGrid of
                        Just location ->
                            let
                                cell = LinkedGrid.getContents location
                            in

                            case getApplicableTransform rule cell object of
                                Just kind ->
                                    updateCell kind location
                                        |> LinkedGrid.gridFromLocation

                                _ ->
                                    ruleApplyGrid

                        _ ->
                            emptyGrid -- should never happen!


            in
            List.foldl ruleFold objFoldGrid pos -- work out how to handle negatives next

    in
    Cell.foldObjects objFold grid grid

{-
    so 9 possibilities
    e.g. two spaces left, two possibilities, both can be win/lose/draw
        win is one move, draw is both moves
        0 if either is win, 2 if either is draw otherwise Nothing

        four spaces left, go through all pairs of moves:
                win: return 0
                min of 4 if loss and recurse: 
                    0 and 2 possible only if all 0s/2s
                    e.g. what does combination of 0 and 2 mean? 
-}

-- best to least good: 0 forced win, 1 win, 2 forced draw, 3 draw, 4 lose

nAndC = 7
