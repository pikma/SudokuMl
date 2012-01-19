type solution = Empty | Solution of Board.gameState ;;

let rec find_solution gameState =
    let legalMoves = Board.legalMoves gameState in
    let aux sol move =
        match sol with
        | Solution(state) -> sol
        | Empty -> find_solution (Board.nextState gameState move) in
    List.fold_left aux Empty legalMoves
;;

