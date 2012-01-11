type gameState
type move
val initialState : gameState
val legalMoves : gameState -> move list
type status = Win | Lose | Playing
val getStatus : gameState -> status
