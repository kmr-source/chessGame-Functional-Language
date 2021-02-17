import Data.List
import Data.Maybe


-- Pieces
data Piece = Pawn | Knight | Bishop | Rook | Queen | King
             deriving (Eq,Ord,Show,Enum)
-- Chess Board
type Board = [[Piece]]

-- Movement of Piece
type Move = ((Char,Int),(Char,Int))

-- List of Pieces
pieces :: [Piece]
pieces = [Pawn .. King]

initialBoard :: Board
initialBoard = [[c] | c<-pieces, _<-[1..6]]