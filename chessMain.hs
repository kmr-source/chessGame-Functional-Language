import Data.List
import Data.Maybe
import Data.Char --toUpper ord

-- Pieces
data Kind = Pawn | Knight | Bishop | Rook | Queen | King
            deriving (Eq,Ord,Show,Enum)
--                   Kind, Board Position, Board Position, Black/White (white = 0)
data Piece = Empty 
            | Create Kind Int
            deriving (Eq,Ord)
            
instance Show Piece where
    show Empty = show "   "
    show (Create k c) = show (if (c==1) then images!!((fromEnum k)+6) else images!!(fromEnum k))

-- Piece Images
images = ["WP ","WN ","WB ","WR ","WQ ","WK ","BP ","BN ","BB ","BR ","BQ ","BK "] -- I am using WSL, doesn't support chess image unicode :(

-- Chess Board
data Board = Initiate [[Piece]]

instance Show Board where 
    show (Initiate ((h:t):tail)) = do
        show (h:t) ++ "\n" ++ show (Initiate tail)
    show (Initiate _) = ""

-- List of Pieces A-H, 1-8, top left is A1
initial = Initiate [[Create Rook 0, Create Knight 0, Create Bishop 0, Create Queen 0, Create King 0, Create Bishop 0, Create Knight 0, Create Rook 0],
    [Create Pawn 0, Create Pawn 0, Create Pawn 0, Create Pawn 0, Create Pawn 0, Create Pawn 0, Create Pawn 0, Create Pawn 0],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Create Pawn 1, Create Pawn 1, Create Pawn 1, Create Pawn 1, Create Pawn 1, Create Pawn 1, Create Pawn 1, Create Pawn 1],
    [Create Rook 1, Create Knight 1, Create Bishop 1, Create Queen 1, Create King 1, Create Bishop 1, Create Knight 1, Create Rook 1]]

--Used https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell
moveto m x (r,c) = 
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m
move board x0 y0 x1 y1 = (moveto (moveto board Empty (y0, x0)) (board!!y0!!x0) (y1,x1)) -- Moves piece from (x0,y0) to (y0,y1)
-- Processing input
extractChar (h:t) = ord (toUpper h) - 65    -- Ensures that lowercase works too
extractNum (h:m:t) = ord m -48 -1 -- Chess boards do 1 indexing, haskell lists do 0, so I converted to 0 in the code
--Viable moves
getColor Empty = 3
getColor (Create a c) = c
checkKing (Initiate [[]]) _ = False
checkKing (Initiate ((h:t):tail)) c 
    | tail == [] = elem (Create King c) (h:t)
    | otherwise = elem (Create King c) (h:t) || checkKing (Initiate tail) c
-- Restriction of movements of pieces
existsIn y x y0 x0(Initiate board) = elem (y0,x0) (checkViableMoves (Initiate board) [] (board!!(y)!!(x)) y x )

-- White pawns
checkViableMoves (Initiate board) _ (Create Pawn 0) 7 x = []
checkViableMoves (Initiate board) _ (Create Pawn 0) y x = 
    if (board!!(y+1)!!x==Empty )
    then
        (y+1,x):(checkPawnWhite (Initiate board) y x)
    else 
        (checkPawnWhite (Initiate board) y x)
-- Black pawns
checkViableMoves (Initiate board) _ (Create Pawn 1) 0 x = []
checkViableMoves (Initiate board) _ (Create Pawn 1) y x = 
    if (board!!(y-1)!!x==Empty )
    then
        (y-1,x):(checkPawnBlack (Initiate board) y x)
    else 
        (checkPawnBlack (Initiate board) y x)
-- Knight 
checkViableMoves (Initiate board) _ (Create Knight c) y x =
    mapAccountForFriendly y x knightMoves (Initiate board) c
-- King
checkViableMoves (Initiate board) _ (Create King c) y x =
    mapAccountForFriendly y x kingMoves (Initiate board) c
-- Bishop
checkViableMoves (Initiate board) _ (Create Bishop c) y x =
    moveCollision y x initial bishopMoves (Create Bishop c)
-- Rook
checkViableMoves (Initiate board) _ (Create Rook c) y x =
    moveCollision y x initial rookMoves (Create Rook c)
-- Queen
checkViableMoves (Initiate board) _ (Create Queen c) y x =
    moveCollision y x initial queenMoves (Create Queen c)
checkViableMoves _ _ _ _ _ =  [(x,y) | x <- [0..7], y <- [0..7]] -- TEMP, REMOVE WHEN DONE, THIS ALLOWS THE REMAINING PIECES TO MOVE HOWEVER
-- HELPER FUNCTIONS
-- White pawn
checkPawnWhite (Initiate board) y x 
    | (y == 1) = if (getColor (board!!(y+1)!!(x)) /= 3) then [] else if (getColor (board!!(y+2)!!(x)) == 3) then [(y+1,x),(y+2,x)] else [(y+1,x)] -- Starting move is 2 steps
    | x == 0 = if (getColor (board!!(y+1)!!(x+1))==1 ) then [(y+1,x+1)] else [] -- if its at the edge of the board
    | x == 7 = if (getColor (board!!(y+1)!!(x-1))==1 ) then [(y+1,x-1)] else [] -- also edge of board
    | (getColor (board!!(y+1)!!(x-1))==1 && getColor (board!!(y+1)!!(x+1))==1) = [(y+1,x+1),(y+1,x+1)] 
    | getColor (board!!(y+1)!!(x-1))==1 = [(y+1,x-1)]
    | getColor (board!!(y+1)!!(x+1))==1 = [(y+1,x+1)]
    | otherwise = []
-- Black pawn
checkPawnBlack (Initiate board) y x 
    | (y == 6) = if (getColor (board!!(y-1)!!(x)) /= 3) then [] else if (getColor (board!!(y-2)!!(x)) == 3) then [(y-1,x),(y-2,x)] else [(y-1,x)] -- Starting move is 2 steps
    | x == 0 = if (getColor (board!!(y-1)!!(x+1))==0 ) then [(y-1,x+1)] else [] -- if its at the edge of the board
    | x == 7 = if (getColor (board!!(y-1)!!(x-1))==0 ) then [(y-1,x-1)] else [] -- also edge of board
    | (getColor (board!!(y-1)!!(x-1))==0 && getColor (board!!(y-1)!!(x+1))==0) = [(y-1,x-1),(y-1,x+1)] 
    | getColor (board!!(y-1)!!(x-1))==0 = [(y-1,x-1)]
    | getColor (board!!(y-1)!!(x+1))==0 = [(y-1,x+1)]
    | otherwise = []
-- Knight
knightMoves :: [(Int,Int)]
knightMoves = [(2,1),(2,-1),(-2,1),(-2,-1),(-1,2),(-1,-2),(1,-2),(1,2)]
kingMoves :: [(Int,Int)] 
kingMoves = delete (0,0) [(x,y)|x <- [-1..1], y <- [-1..1]] -- all combinations of a 3x3 except for staying still
bishopMoves :: [(Int,Int)]
bishopMoves = [(x,y)|x <- [-1,1], y <- [-1,1]]
rookMoves :: [(Int,Int)]
rookMoves = [(1,0),(-1,0),(0,1),(0,-1)]
queenMoves :: [(Int,Int)]
queenMoves = bishopMoves++rookMoves
-- HELPER FOR ALL PIECES
addCoords y x (y1,x1) = (y1+y,x1+x)
removeBounds (y,x) = if (y < 0 || x < 0 || y > 7 || x > 7) then False else True
mapMoves y x moves = filter removeBounds (nub(map (addCoords y x) moves))   
mapAccountForFriendly y x moves (Initiate board) c = filter (\(y,x) -> getColor(board!!(y)!!(x))/= c) (mapMoves y x moves)
-- y x is coordinate of unit, (y0,x0) is the movement (1,0) is up, (0,1) is right, returns a list of moves
-- recursively checks the direction until you hit a piece/border
checkCollision y x (Initiate board) c (y0,x0) 
    | (y <= 0 || x <= 0 || y >= 7 || x >= 7) = []
    | (getColor (board!!(y+y0)!!(x+x0)))== c = []
    | (getColor (board!!(y+y0)!!(x+x0)))== 3 = (y0+y,x0+x):checkCollision (y+y0) (x+x0) (Initiate board) c (y0,x0)
    | otherwise = [(y0+y,x0+x)]
moveCollision y x (Initiate board) moves (Create _ c) = nub (concat(map (checkCollision y x (Initiate board) c) moves ))
-- Main game (for white player)
play (Initiate board) 0 = 
    do
      putStrLn (show (Initiate board))
      putStrLn "Enter Player 1 Piece location"
      ans <- getLine
      if ((length ans) <2) then do
          putStrLn "Not a valid piece, must be of form 'a1' and be a white piece"
          play (Initiate board) 0
      else do
          let x = extractChar ans --Will be represented by 0-7
          let y = extractNum ans --Will be represented by 0-7
          if (x >=0 && x <= 7 && y >=0 && y <= 7 && board!!y!!x /= Empty && getColor(board!!y!!x) /= 1 ) 
            then do 
                putStrLn "Enter Player 1 movement location"
                ans1 <- getLine
                let x1 = extractChar ans1
                let y1 = extractNum ans1
                if (x1 >=0 && x1 <= 7 && y1 >=0 && y1 <= 7 && (existsIn y x y1 x1 (Initiate board))) 
                    then do
                        let nextTurn = Initiate(move board x y x1 y1)
                        if (checkKing nextTurn 1)
                        then
                            play (nextTurn) 1
                        else 
                            return "White Wins"
                    else do
                        putStrLn "Invalid Move"
                        play (Initiate board) 0
            else do
                putStrLn "Not a valid piece, must be of form 'a1' and be a white piece"
                play (Initiate board) 0
-- (Main game for black player)
play (Initiate board) 1 = 
    do
      putStrLn (show (Initiate board))
      putStrLn "Enter Player 2 Piece location"
      ans <- getLine
      if ((length ans) <2) then do
          putStrLn "Not a valid piece, must be of form 'a1' and be a black piece"
          play (Initiate board) 1
      else do 
          let x = extractChar ans --Will be represented by 0-7
          let y = extractNum ans --Will be represented by 0-7
          if (x >=0 && x <= 7 && y >=0 && y <= 7 && board!!y!!x /= Empty && getColor(board!!y!!x) /= 0)
            then do 
                putStrLn "Enter Player 2 movement location"
                ans1 <- getLine
                let x1 = extractChar ans1
                let y1 = extractNum ans1
                if (x1 >=0 && x1 <= 7 && y1 >=0 && y1 <= 7) 
                    then do
                        let nextTurn = Initiate(move board x y x1 y1 )
                        if (checkKing nextTurn 0)
                        then
                            play (nextTurn) 0
                        else 
                            return "Black Wins"
                    else do
                        putStrLn "Invalid Move"
                        play (Initiate board) 1
            else do
                putStrLn "Not a valid piece, must be of form 'a1' and be a black piece"
                play (Initiate board) 1
go = (play initial 0)
