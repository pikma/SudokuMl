type gameState
type move
type status = Win | Lose | Playing
val initialState : gameState
val legalMoves : gameState -> move list
val getStatus : gameState -> status
val nextState : gameState -> move -> gameState
