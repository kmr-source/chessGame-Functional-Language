# Chess-Project
chessMain is one without audio built in, chessP has audio

# ♟️ Haskell Chess Engine

A functional, terminal-based chess game written in **Haskell**, featuring complete rule logic for standard chess pieces and basic turn-based play between two players. 

This engine simulates a chessboard, enforces movement rules (including captures, check detection, and piece restrictions), and allows players to interact via terminal input in algebraic coordinates (e.g., `e2`, `g8`).

---

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

### Compile with GHC:

```bash
ghc -o chess Main.hs
