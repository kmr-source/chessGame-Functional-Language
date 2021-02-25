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

-- List of Pieces
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
-- Adding delete input
fixdel (h:t) =
    if notDone (h:t) >= 1
        then fixdel (remove (h:t))
            else (h:t)
substring start end text = (take (end - start) (drop start text))
notDone (h:t) 
    | length (h:t) < 2 = 0
    | substring 1 2 (h:t) == "\DEL" = 1
    | otherwise = notDone t 
remove (h:t)
    | length (h:t) < 2 = (h:t)
    | substring 1 2 (h:t) == "\DEL" = substring 2 (length (h:t)) (h:t)
    | otherwise = h : remove (t)
--Viable moves
getColor (Create _ c) = c
checkKing (Initiate [[]]) _ = False
checkKing (Initiate ((h:t):tail)) c 
    | tail == [] = elem (Create King c) (h:t)
    | otherwise = elem (Create King c) (h:t) || checkKing (Initiate tail) c

-- Main game (for white player)
play (Initiate board) 0 = 
    do
      putStrLn (show (Initiate board))
      putStrLn "Enter Player 1 Piece location"
      lin <- getLine
      let ans = fixdel lin
      if ((length ans) <2) then do
          putStrLn "Not a valid piece, must be of form 'a1' and be a white piece"
          play (Initiate board) 0
      else do
          let char = extractChar ans --Will be represented by 0-7
          let num = extractNum ans --Will be represented by 0-7
          if (char >=0 && char <= 7 && num >=0 && num <= 7 && board!!num!!char /= Empty && getColor(board!!num!!char) /= 1) 
            then do 
                putStrLn "Enter Player 1 movement location"
                lin1 <- getLine
                let ans1 = fixdel lin1
                let char1 = extractChar ans1
                let num1 = extractNum ans1
                if (char1 >=0 && char1 <= 7 && num1 >=0 && num1 <= 7) 
                    then do
                        let nextTurn = Initiate(move board char num char1 num1)
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
      lin <- getLine
      let ans = fixdel lin
      if ((length ans) <2) then do
          putStrLn "Not a valid piece, must be of form 'a1' and be a black piece"
          play (Initiate board) 1
      else do 
          let char = extractChar ans --Will be represented by 0-7
          let num = extractNum ans --Will be represented by 0-7
          if (char >=0 && char <= 7 && num >=0 && num <= 7 && board!!num!!char /= Empty && getColor(board!!num!!char) /= 0)
            then do 
                putStrLn "Enter Player 2 movement location"
                lin1 <- getLine
                let ans1 = fixdel lin1
                let char1 = extractChar ans1
                let num1 = extractNum ans1
                if (char1 >=0 && char1 <= 7 && num1 >=0 && num1 <= 7) 
                    then do
                        let nextTurn = Initiate(move board char num char1 num1 )
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
