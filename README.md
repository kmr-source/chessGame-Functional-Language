# Chess-Project
chessMain is one without audio built in, chessP has audio

# ♟️ Haskell Chess Engine

A functional, terminal-based chess game written in **Haskell**, featuring complete rule logic for standard chess pieces and basic turn-based play between two players. We will be implementing a project based on the game of Chess.  This will be testing the use of real time user inputs, iterative game states, and applying arbitrary rules in haskell.

This engine simulates a chessboard, enforces movement rules (including captures, check detection, and piece restrictions), and allows players to interact via terminal input in algebraic coordinates (e.g., `e2`, `g8`).

What libraries are we going to use?

Euterpea library : http://euterpea.com/

Euterpea Library works best on Haskell platform 8.6.5  && cabal version <3.1

Instructions with for cabal > 3.1

cabal v1-update

cabal v1- install Euterpea

*******************Warning* if above does not work it may be best to reinstall Haskel Platform 8.6.5***********************

Please install Haskell Platform 8.6.5 --- link: www.haskell.org/platform/prior.html

What functionality do we have (basic)

Two player interface
Enforced movement based off game rules in the chess game
What is the something extra?
We will be implementing sounds for the game, such as on player movements, game starting, taking pieces, game ending, and invalid movement sounds .

What did we learn from doing this?

We learned that while Haskell was a relatively good and feasible language for taking on games, but the inability to rewrite variables occasionally causes a lot of inefficiencies, such as having to rewrite the entire board state instead of changing a single value within it. In addition, we learned that due to how overloading can a very simple way to have different functions for each data class instead of the usual polymorphism and overriding like in a OOP focused language. In the end, we made the board a list of a list of piece types, which contains the usual pieces (pawn,rook, queen etc) which all have a constructor which contains a color, and also a constant that has a space. This was a very simple way to implement the game because for each type of piece, we can check the type of piece in a OOP like way, while also being able to omit things like the piece type when specifically looking for color, and such, making it easier to code than OOP because of the reduced redundancies. In addition, we also were able to reuse functions which calculated potential moves for different pieces, something which would be avoided in OOP, due to the increased complexity. While attempting to implement the sound for the sound effects of the game, we learned that Haskell could easily use Math functions like sin to create wave frequencies which could be used to make a muscial tune. The waves could be created using simple Int, list, zip functions, after attemping to use the ffmpeg library for the sound it was found that the library was incompatible with our code as it had a windown console constantly appeared. We then chose to use Euterpea library instead it's library to concat simple lists together to create simple notes and tunes.


## 🧠 Features

- Full board and piece representation using algebraic data types
- Rules implemented for:
  - **All standard pieces** (Pawn, Rook, Knight, Bishop, Queen, King)
  - Movement, attack, and capture logic
  - Pawn promotions (basic support)
  - King-check validation
- Text-based interface (no external GUI required)
- Uses ASCII-style piece symbols for simplicity

---

## 🛠 Built With

- **GHC (Glasgow Haskell Compiler)**
- No external Haskell libraries needed
- Purely functional design using standard Prelude

---

## ▶️ Running the Game

Make sure GHC is installed:

### Compile with GHC:

```bash
ghc -o chess Main.hs

./chess


