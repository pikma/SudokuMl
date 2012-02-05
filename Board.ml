module Position =
    struct
        type t = int * int
        let compare t1 t2 =
            match t1 with
            (v11, v12) ->
                match t2 with
                (v21, v22) -> if (v11 < v21) then -1
                else if (v11 > v21) then 1
                else if (v12 < v22) then -1
                else if (v12 > v22) then 1
                else 0
    end;;


type position = Position.t
type cellNumber = int
type cellContent = Number of cellNumber | NoNumber
type status = Win | Lose | Playing
type move = (position * cellNumber)
module PositionMap = Map.Make(Position)
type gameState = cellContent PositionMap.t
type zone = position list

(* List of integers from a to b, inclusive *)
let range a b =
    let rec aux c d l =
        if c > d then l
        else aux c (d-1) (d::l) in
    aux a b [] ;;

let allNumbers =
    let allInts = range 1 9 in
    List.map (function x -> Number(x)) allInts ;;

(* List of all the positions on the board *)
let allPositions =
    let aux2 first acc last =
        (first, last) :: acc in
    let allInts = range 1 9 in
    let aux acc n =  (* append (n,1), (n,2) ... to acc *)
        List.fold_left (aux2 n) acc allInts in
    List.fold_left aux [] allInts ;;

let isNumber cellContent =
    match cellContent with
            | Number(_) -> true
            | NoNumber -> false ;;

(* Returns true if a list is unique *)
let rec isUniq lst =
    match lst with
    | [] -> true
    | hd :: tl ->
          match tl with
          | [] -> true
          | hhd :: ttl -> if hd = hhd then false
                            else isUniq tl ;;

(* if ll = [a;b] and lr = [c;d], returns [[(a,c);(a,d)]; [(b,c);(b,d)]] *)
let left_cross ll lr =
    let aux el =
        List.map (function x -> (el, x)) lr in
    List.map aux ll
;;

let right_cross ll lr =
    let aux er =
        List.map (function x -> (x, er)) ll in
    List.map aux lr
;;

let cross_product ll rr =
    List.flatten (left_cross ll rr)
;;

let allZones =
    let allIndices = range 1 9 in
    let lines = left_cross allIndices allIndices in
    let columns = right_cross allIndices allIndices in
    let z = [range 1 3; range 4 6; range 7 9] in
    let rangePairs = cross_product z z in
    let aux rangePair =
        match rangePair with
        (range1,range2) -> cross_product range1 range2 in
    let squares = List.map aux rangePairs in
    List.concat [lines; columns; squares]
;;

(* Returns the cell content of a given position *)
let getCellContent gameState pos =
    PositionMap.find pos gameState ;;

(* Return true if the rules of the sudoku are respected *)
let isCorrect gameState =
    let numbersInZone gameState zone =
        let cellContents = List.map (getCellContent gameState) zone in
        List.filter isNumber cellContents in
    List.for_all isUniq (List.map (numbersInZone gameState) allZones) ;;

(* Returns true if the position is filled with a number *)
let isFilled gameState pos =
    let cellContent = PositionMap.find pos gameState in
    isNumber cellContent ;;

let getStatus gameState =
    if List.for_all (isFilled gameState) allPositions then
        if isCorrect gameState then Win else Lose
    else
        Playing ;;

exception EmptyList;;

(* Return the minimum scoring element of a list *)
let min score l =
    match l with
    | [] -> raise EmptyList
    | hd :: tl ->
            let min_aux a b =
                if score a < score b then a else b in
            List.fold_left min_aux hd tl ;;


(* Returns a list where all the elements of l appear exactly once. Nees a
 * comparator to sort the list. *)
let uniq comparator l =
    let rec aux sortedList =
        match sortedList with
        | [] -> []
        | hd :: tl ->
                match tl with
                | [] -> [hd]
                | hhd :: ttl -> if hd = hhd then aux tl
                                else hd :: aux tl in
    let sList = List.sort comparator l in
    aux sList
;;

exception NoNumberException ;;

(* Returns the legal moves for a given position *)
let legalMovesPos gameState pos =
    let myZones = List.filter (List.mem pos) allZones in
    let concurrentPos = uniq Position.compare (List.flatten myZones) in
    let concurrentContents = List.map (getCellContent gameState) concurrentPos in
    let concurrentNumbers = uniq Pervasives.compare (List.filter isNumber concurrentContents) in
    let filterAux = function n -> not (List.mem n concurrentNumbers) in
    let resultContents = List.filter filterAux allNumbers in
    let mappingAux = function content -> match content with
        | NoNumber -> raise NoNumberException
        | Number(x) -> x in
    List.map mappingAux resultContents
;;

(* Picks one unfilled position, and returns all the possible moves for that
 * position *)
let legalMoves gameState =
    (* This auxilliary function finds the position with the smallest number of
     * possible moves. It returns the position and its legal moves. *)
    let rec aux minPosition minLegalMoves minNbLegalMoves positions =
        match positions with
        | [] -> (minPosition, minLegalMoves)
        | hd :: tl ->
                let currentLegalMoves = legalMovesPos gameState hd in
                let currentNbLegalMoves = List.length currentLegalMoves in
                if currentNbLegalMoves < minNbLegalMoves then
                    if currentNbLegalMoves <= 1 then (hd, currentLegalMoves)
                    else aux hd currentLegalMoves currentNbLegalMoves tl
                else
                    aux minPosition minLegalMoves minNbLegalMoves tl in

    let emptyPositions = List.filter (isFilled gameState) allPositions in
    let res = aux (1,1) [] (List.length allNumbers + 1) emptyPositions in
    match res with
    | (pos, possibleContents) ->
            List.map (function content -> (pos, content)) possibleContents
;;

let initialState = PositionMap.empty ;;

let printPosition p =
    match p with
    (u,v)-> print_string "[";
            print_int u;
            print_string ", ";
            print_int v;
            print_string "]"
;;

exception EmptyContentInMove ;;

let nextState gameState move =
    match move with
    | (pos, number) -> PositionMap.add pos (Number(number)) gameState
;;
