getSecondElement :: [a] -> a
getSecondElement (_ : x : _) = x

countInstances :: Eq a => a -> [a] -> Int
countInstances value list = length (filter (== value) list)

positionAtBoard :: Int -> String
positionAtBoard coord
    | coord `elem` [1, 8] = "Outer Edge"
    | coord `elem` [2, 7] = "Inner Edge"
    | coord `elem` [3, 6] = "Outer Center"
    | otherwise = "Inner Center"

coordinateCombination :: Int -> Int -> [String]
coordinateCombination column row =  do
    [columnPosition, rowPosition]
    where
        columnPosition = positionAtBoard column
        rowPosition = positionAtBoard row

atOuterEdge :: String -> Bool
atOuterEdge position = position == "Outer Edge"

atInnerEdge :: String -> Bool
atInnerEdge position = position == "Inner Edge"

atOuterCenter :: String -> Bool
atOuterCenter position = position == "Outer Center"

atInnerCenter :: String -> Bool
atInnerCenter position = position == "Inner Center"

numOfMovesToCheck :: Int -> Int -> String -> Int
-- Assuming that the board is empty
-- Knight
numOfMovesToCheck column row "Knight"
    | count "Outer Edge" == 2 = 2
    | count "Inner Edge" == 1 && count "Outer Edge" == 1 = 3
    | count "Inner Edge" == 2 || (count "Outer Edge" == 1 && count "Center" == 1) = 4
    | count "Inner Edge" == 1 && count "Center" == 1 = 6
    | otherwise = 8
    where
        combination = coordinateCombination column row
        count position = countInstances position combination
-- Pawn
numOfMovesToCheck column row "Pawn" = 1
-- King
numOfMovesToCheck column row "King"
    | count "Outer Edge" == 2 = 3
    | count "Inner Edge" == 1 && count "Outer Edge" == 1 = 5
    | otherwise = 8
    where
        combination = coordinateCombination column row
        count val = countInstances val combination
-- Rook
numOfMovesToCheck column row "Rook" = 14
-- Bishop
numOfMovesToCheck column row "Bishop"
    | atOuterEdge colPos || atOuterEdge rowPos = 7
    | atInnerEdge colPos || atInnerEdge rowPos = 9
    | atOuterCenter colPos || atOuterCenter rowPos = 11
    | otherwise = 13
    where
        colPos = positionAtBoard column
        rowPos = positionAtBoard row
-- Queen
numOfMovesToCheck column row "Queen" = numOfBishopMoves + numOfRookMoves
    where
        numOfBishopMoves = numOfMovesToCheck column row "Bishop"
        numOfRookMoves = numOfMovesToCheck column row "Rook"
