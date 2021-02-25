import Data.List
import Data.Maybe
import Data.Char --toUpper ord
import Control.Concurrent 
import Euterpea 

--http://hackage.haskell.org//base-4.12.0.0/docs/Control-Concurrent.html
-- credit: https://stackoverflow.com/questions/54865136/start-euterpea-music-in-main-function-together-with-gameworld-in-haskell

--main = do musicThreadId <- forkIO $ Euterpea.play $ Euterpea.line [af 4 dqn :=: cf 4 dqn :=: ef 4 dqn] 
melody :: Music Pitch
melody = line [c 5 qn, g 5 qn, g 5 qn, c 5 qn, g 5 qn, g 4 hn, c 4 qn, g 5 qn, g 5 qn, c 3 qn, g 3 qn, g 5 hn]
chords :: Music Pitch
chords = chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 hn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ c 3 hn, d 4 hn, e 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 hn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ c 3 hn, d 4 hn, e 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 hn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ c 3 hn, d 4 hn, e 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 hn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ c 3 hn, d 4 hn, e 3 hn] :+:
		 chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 qn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 qn, g 3 wn] :+:
         chord [ c 3 hn, f 3 qn, a 3 hn] :+:
         chord [ c 3 qn, d 4 hn, e 3 wn]
twinkle :: Music Pitch
twinkle = melody :=: chords :=: melody

melody2 :: Music Pitch
melody2 = Euterpea.line [af 5 dqn :=: cf 4 dqn]

endGameMelody :: Music Pitch
endGameMelody = chord [ e 5 wn, c 3 wn, d 3 wn, e 5 wn, c 5 en]

musicBeginning :: Music Pitch
musicBeginning = line [a 5 qn, b 5 qn, c 5 qn, a 5 qn, b 5 qn, c 4 hn, a 4 qn, e 5 qn, g 5 qn, b 3 qn, c 3 wn]

muscEnd :: Music Pitch
muscEnd = line [e 5 qn, e 5 qn, e 5 qn, d 5 qn, c 5 qn, c 6 hn]


-- Pieces
data Kind = Pawn | Knight | Bishop | Rook | Queen | King
            deriving (Eq,Ord,Show,Enum)
--                   Kind, Board Position, Board Position, Black/White (white = 0)
data Piece = Empty 
            | Create Kind Char Int Int
            deriving (Eq,Ord)
            
instance Show Piece where
    show Empty = show "   "
    show (Create k _ _ c) = show (if (c==1) then images!!((fromEnum k)+6) else images!!(fromEnum k))

-- Piece Images
images = ["WP ","WN ","WB ","WR ","WQ ","WK ","BP ","BN ","BB ","BR ","BQ ","BK "] -- I am using WSL, doesn't support chess image unicode :(

-- Chess Board
data Board = Initiate [[Piece]]

instance Show Board where 
    show (Initiate ((h:t):tail)) = do
        show (h:t) ++ "\n" ++ show (Initiate tail)
    show (Initiate _) = ""

-- List of Pieces
initial = Initiate [[Create Rook 'A' 8 0, Create Knight 'B' 8 0, Create Bishop 'C' 0 0, Create Queen 'D' 8 0, Create King 'E' 8 0, Create Bishop 'F' 8 0, Create Knight 'G' 8 0, Create Rook 'H' 8 0],
    [Create Pawn 'A' 7 0, Create Pawn 'B' 7 0, Create Pawn 'C' 7 0, Create Pawn 'D' 7 0, Create Pawn 'E' 7 0, Create Pawn 'F' 7 0, Create Pawn 'G' 7 0, Create Pawn 'H' 7 0],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Create Pawn 'A' 2 1, Create Pawn 'B' 2 1, Create Pawn 'C' 2 1, Create Pawn 'D' 2 1, Create Pawn 'E' 2 1, Create Pawn 'F' 2 1, Create Pawn 'G' 2 1, Create Pawn 'H' 2 1],
    [Create Rook 'A' 1 1, Create Knight 'B' 1 1, Create Bishop 'C' 1 1, Create Queen 'D' 1 1, Create King 'E' 1 1, Create Bishop 'F' 1 1, Create Knight 'G' 1 1, Create Rook 'H' 1 1]]

--Used https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell
moveto m x (r,c) = 
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m
move board x0 y0 x1 y1 = (moveto (moveto board Empty (y0, x0)) (board!!y0!!x0) (y1,x1))
-- Processing input
extractChar (h:t) = ord (toUpper h) - 65    -- Ensures that lowercase works too
extractNum (h:m:t) = ord m -48 -1 -- Chess boards do 1 indexing, haskell lists do 0, so I converted to 0 in the code
-- Adding delete input
fixdel (h:t) =
    if notDone (h:t) >= 1
        then fixdel (Main.remove (h:t))
            else (h:t)
substring start end text = (take (end - start) (drop start text))
notDone (h:t) 
    | length (h:t) < 2 = 0
    | substring 1 2 (h:t) == "\DEL" = 1
    | otherwise = notDone t 
remove (h:t)
    | length (h:t) < 2 = (h:t)
    | substring 1 2 (h:t) == "\DEL" = substring 2 (length (h:t)) (h:t)
    | otherwise = h : Main.remove (t)
getColor (Create _ _ _ c) = c
-- Main game (for white player)
play2 (Initiate board) 'w' = 
    do
      putStrLn (show (Initiate board))
      putStrLn "Enter Player 1 Piece location"
      lin <- getLine
      let ans = fixdel lin
      let char = extractChar ans --Will be represented by 0-7
      let num = extractNum ans --Will be represented by 0-7
      if (((((char >=0) && (char <= 7)) && (num >=0)) && (num <= 7)) && board!!num!!char /= Empty && getColor(board!!num!!char) /= 1) 
        then do 
            putStrLn "Enter Player 1 movement location"
            lin1 <- getLine
            let ans1 = fixdel lin1
            let char1 = extractChar ans1
            let num1 = extractNum ans1
            if ((((char >=0) && (char <= 7)) && (num1 >=0)) && (num1 <= 7)) 
                then do
                    let nextTurn = Initiate(move board char num char1 num1 )
                    putStrLn (show(nextTurn))
                    play2 (nextTurn) 'b'
				    --playDev 0 (melody2)
                else do
                    putStrLn "Invalid Move"
                    play2 (Initiate board) 'w'
        else do
            putStrLn "Not a valid piece, must be of form 'a1' and be a white piece"
            play2 (Initiate board) 'w'
-- (Main game for black player)
play2 (Initiate board) 'b' = 
    do
      putStrLn (show (Initiate board))
      putStrLn "Enter Player 2 Piece location"
      lin <- getLine
      let ans = fixdel lin
      let char = extractChar ans --Will be represented by 0-7
      let num = extractNum ans --Will be represented by 0-7
      if (((((char >=0) && (char <= 7)) && (num >=0)) && (num <= 7)) && board!!num!!char /= Empty && getColor(board!!num!!char) /= 0) 
        then do 
            putStrLn "Enter Player 2 movement location"
            lin1 <- getLine
            let ans1 = fixdel lin1
            let char1 = extractChar ans1
            let num1 = extractNum ans1
            if ((((char >=0) && (char <= 7)) && (num1 >=0)) && (num1 <= 7)) 
                then do
                    let nextTurn = Initiate(move board char num char1 num1 )
                    putStrLn (show(nextTurn))
                    play2 (nextTurn) 'w'
                else do
                    putStrLn "Invalid Move"
                    play2 (Initiate board) 'b'
        else do
            putStrLn "Not a valid piece, must be of form 'a1' and be a black piece"
            play2 (Initiate board) 'b'
go = do play melody
        do play2 initial 'w'

--main :: IO()
--main = do playDev 0 
          
--main :: IO ()
--main = do
--    putStrLn "Enter yes to play the game and no to exit"
--    do
--        line1 <- getLine                                
--        do 
--	      if (line1 == "yes")
--		    then play melody
--		  else putStrLn "goodbye"

main2 :: IO ()
main2 = do
    putStrLn "Welcome to our Chess Game"
    putStrLn "Would you like to start the game? please Enter yes to start and no to Exit"
    do
        line1 <- getLine                                -- line1 :: String
        do 
	      if (line1 == "yes")
		    then go     
		  else
		     do putStrLn "Game has ended"
		        play muscEnd
         
       