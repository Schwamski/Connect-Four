open!  Setup;   
open Game; 
module Connect4 = {
/*------------------------------------------------------------------------------
whichPlayer (P1 is player one, P2 is player two)
------------------------------------------------------------------------------*/
    // player 1 is P1, player 2 is P2 
    // Example Data:
    // P1; P2
    type whichPlayer =
      | P1
      | P2;
/*------------------------------------------------------------------------------
status (a player has one, the game is a draw, or the game is ongoing)
------------------------------------------------------------------------------*/
    // either a player has won, it's a draw, or it's ongoing 
    // Example Data: 
    // Win(P2); Draw; Ongoing(P1)
    type status =
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);
/*------------------------------------------------------------------------------
state
------------------------------------------------------------------------------*/
// expresses the state of the game, including the game status and the state of
// the game board, represented as a matrix (list(list(int)))
// Example Data:
// State(Ongoing(P1), [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0],
// [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])
    type state =
      | State(status, list(list(int)));
/*------------------------------------------------------------------------------
move
------------------------------------------------------------------------------*/
// describes a move that a player makes which, in the case, of connect 4, is
// just the column that the player drops their chip into
// Example Data:
// Column(0); Column(3); Column(6)
    type move =
      | Column(int);
/*------------------------------------------------------------------------------
currentPlayer
------------------------------------------------------------------------------*/
// input: a state
// output: a whichPlayer indicating whose turn it is
    let currentPlayer: state => whichPlayer = inState =>
    switch (inState) {
      | State(Draw, _) => failwith("currentPlayer domain error")
      | State(Ongoing(p), _) => p
      | State(Win(p), _) => p
      };
/*------------------------------------------------------------------------------
otherPlayer
------------------------------------------------------------------------------*/
// input: a player
// output: the opposing player
    let otherPlayer = (player: whichPlayer): whichPlayer =>
      switch (player) {
      | P1 => P2
      | P2 => P1
      };
/*------------------------------------------------------------------------------
initialState
------------------------------------------------------------------------------*/
// initialStateHelper
// input: a single value of type 'a, a, and an int, n,
// output: a list of a repeated n times
// Recursion Diagram #1:
// OI: (7, 0)
//     RI: N/A
//     RO: N/A
// OO: []
//
// Recursion Diagram #2:
// OI: (7, 3)
//     RI: (7, 2)
//     RO: [7, 7]
//     Ideation: Cons an additional 7 onto the RO and reduce the value of n by 1
// OO: [7, 7, 7]
    let rec initialStateHelper: ('a, int) => list('a) = (a, n) =>
        if (n <= 0) {
          [];
        } else {
          [a, ...initialStateHelper(a, n-1)];
        };
// initialState
// input: a string containing the board dimensions
// output: a list of lists of 0, where each inner list is a column of the board
    let initialState: string => state =
      s => {
        let boardDims = parseBoardDims(s);
        let boardHeight = getBoardHeight(boardDims);
        let boardWidth = getBoardWidth(boardDims);
        State(Ongoing(P1),
          initialStateHelper(initialStateHelper(0, boardHeight), boardWidth));
      };
/*------------------------------------------------------------------------------
gameStatus
------------------------------------------------------------------------------*/
// input: a game state
// output: the game status extracted from the game state
    let gameStatus: state => status =
    inState => {
      let State(p, _) = inState;
      p;
    };
/*------------------------------------------------------------------------------
checkWinner
------------------------------------------------------------------------------*/
// input: the game board represented as a list(list(int))
// output: a boolean indicating if a player has won
    let rec checkWinner: list(list(int)) => bool = currentBoard =>
      checkVertical(currentBoard) || /* vertical */
      checkVertical(transpose(currentBoard)) || /* horizontal */
      checkVertical(diagonalize(currentBoard)) || /* right diagonal */
      checkVertical(diagonalize(List.rev(currentBoard))) /* left diagonal */
// checkVertical
// input: the game board
// output: a boolean indicating if there exists a vertical four in a row
    and checkVertical: list(list(int)) => bool =
      currentBoard => List.mem(true, List.map(checkVerticalHelper,currentBoard))
// checkVerticalHelper
// input: a list of int
// output: a boolean indicating if there exists a vertical four in a row
// Recursion Diagram for checkVerticalHelper #1
// OI: [1, 1, 1, 1, 0]
// RI: N/A
// RO: N/A
// IS: N/A
// OO: true
// Recursion Diagram for checkVerticalHelper #2
// OI: [1, 1, 0, 1, 0]
// RI: [1, 0, 1, 0]
// RO: false
// IS: since the RO is false (because there is no tail) the OO must be false
// OO: false
// */
    and checkVerticalHelper: list(int) => bool = column =>
      switch(column) {
        | [a, b, c, d, ...tl] =>
          if (a == b && b == c && c == d && d != 0)
            {true}
          else
            {checkVerticalHelper([b,c,d, ...tl])}
        | _ => false
      }
// transpose
// input: a list(list(int))
// output: the transpose of this pseudo-array
    and transpose : list(list(int)) => list(list(int)) =
    currentBoard =>
        switch(currentBoard) {
        | []
        | [[], ..._] => failwith ("the board cannot be zero-dimensional")
        | [[_], ..._] => [List.flatten(currentBoard)]
        | [[_, ..._], ..._] =>
            [List.map(List.hd, currentBoard),
            ...transpose(List.map(List.tl, currentBoard))]
        }
// diagonalize
// input: the game board
// output: a new game board where each NW-to-SE diagonal is converted to a
// vertical column
    and diagonalize: list(list(int)) => list(list(int)) = currentBoard =>
      List.rev(List.tl(diagonalizeHelper(transpose(currentBoard))))
      @ diagonalizeHelper(currentBoard)
// diagonalizeHelper
// input: the game board
// output: a new game board where each NW-to-SE diagonal is converted to a
// vertical column, but only includes the top half of diagonals 
    and diagonalizeHelper: list(list(int)) => list(list(int)) =
      currentBoard =>
      switch(currentBoard) {
        | [] => []
        | [_hd, ...tl] => [getDiagonal(currentBoard), ...diagonalizeHelper(tl)]
      }
// getDiagonal
// input: the game board
// output: extracts the list(int) representing the main diagonal 
    and getDiagonal: list(list(int)) => list(int) = currentBoard =>
      switch(currentBoard) {
        | [] => []
        | [[a1, ..._tl1], ...tl] => [a1, ...getDiagonal(List.map(List.tl, tl))]
        | _ => []
      };
/*------------------------------------------------------------------------------
nextState
------------------------------------------------------------------------------*/
// input: the game state and a move made by a player
// output: the new state created by the player making their move on the previous
// state
    let rec nextState: (state, move) => state =
      (inState, inMove) =>
        switch(inState, inMove) {
        | (State(Win(_), _), _) => inState
        | (State(Draw, _), _) => inState
        | (State(Ongoing(player), currentBoard), Column(col)) =>
          {let updatedBoard = chipDrop(playerToChip(player), col, currentBoard)
          if (checkWinner(updatedBoard))
              {State(Win(player), updatedBoard)}
          else
            if (boardSpaceQ(updatedBoard))
              {State(Ongoing(otherPlayer(player)), updatedBoard)}
            else
              {State(Draw, updatedBoard)}
          };
        }
// chipDrop
// input: an int, player, an int, column, and an int, board
// output: a new board with the player's chip added to the column  
    and chipDrop: (int, int, list(list(int))) => list(list(int)) =
      (player, column, board) =>
        switch(board) {
        | [] => failwith("No Columns Available")
        | [hd, ...tl] when column<=1 => [chipDropHelper(player, hd), ...tl]
        | [hd, ...tl] => [hd, ...chipDrop(player, column-1, tl)]
        }

// chipDropHelper
// input: an int, player, a list(int)
// output: a new list(int) representing the column with the player's chip added
    and chipDropHelper: (int, list(int)) => list (int) = (player, column) =>
      switch(column) {
      | [] => failwith("column of 0 height")
      | [0] => [player]
      | [0, 0, ...tl] => [0, ...chipDropHelper(player,[0, ...tl])]
      | [0, a, ...tl] => [player, a, ...tl]
      | [_a, ..._tl] => failwith("illegal move: column filled")
      }
// playerToChip
// input: a player
// output: an int corresponding to the player
    and playerToChip: whichPlayer => int = player =>
      switch(player){
        | P1 => 1
        | P2 => 2
      }
// boardSpaceQ
// input: the game board
// output: a boolean indicating if any space remains on the board
    and boardSpaceQ: list(list(int)) => bool = updatedBoard =>
      switch(updatedBoard) {
        | [] => false
        | [[0, ..._tl1], ..._tl] => true
        | [[_hd1, ..._tl1], ...tl] => boardSpaceQ(tl)
        | _ => failwith("boardSpaceQ domain error")
      };
/*------------------------------------------------------------------------------
legalMoves
------------------------------------------------------------------------------*/
// input: the game state
// output: a list of legal moves that a player may make 
let rec legalMoves: state => list(move) = inState =>
  {let State(_inStatus, currentBoard) = inState
  legalMovesHelper(legalColumns(currentBoard), 1)}
// legalMovesHelper
// input: a list of booleans and an int
// output: a list of legal moves that a player may make
and legalMovesHelper: (list(bool), int) => list(move) =
  (legalCols, col) => switch(legalCols) {
    | [] => []
    | [false, ...tl] => legalMovesHelper(tl, col+1)
    | [true, ...tl] => [Column(col), ...legalMovesHelper(tl, col+1)]
  }
// legalColumns
// input: a game board
// output: a list of booleans representing which columns have legal moves
and legalColumns: (list(list(int))) => list(bool) = currentBoard =>
  List.map(x => (List.hd(x) == 0), currentBoard)
/*------------------------------------------------------------------------------
estimateValue
------------------------------------------------------------------------------*/
// completeCounter
// input: an int, player, a procedure, proc (either checkTwos or checkThrees),
// and the gameboard as a list(list(int))
// output: an integer indicating the number of times that checkTwos/checkThrees
// is satisfied for the given player on the given board
let rec completeCounter: (int, (int, list(int)) => int, list(list(int))) => int =
  (player, proc, currentBoard) =>
  orientationCounter(player, proc, currentBoard) +
  orientationCounter(player, proc, transpose(currentBoard)) +
  orientationCounter(player, proc, diagonalize(currentBoard)) +
  orientationCounter(player, proc, diagonalize(List.rev(currentBoard)))
// input: an int, p, and a list(int), column
// output: an int indicating how many potential four in a rows exist in the
// column for which exactly two of the four chips have been placed
and checkTwos: (int, list(int)) => int = (p, column) =>
      switch(column) {
        | [0, 0, a, b] =>
          if (a == b && b == p) {1}
          else {0}
        | [0, 0, a, b, c, ...tl] =>
          if (a == b && b == p && c != p) {1 + checkTwos(p, [c, ...tl])}
          else {checkTwos(p, [0, a, b, c, ...tl])}
        | [0, a, b, 0, ...tl] =>
          if (a == b && b == p) {1 + checkTwos(p, tl)}
          else {checkTwos(p, [a, b, 0, ...tl])}
        | [a, b, 0, 0, ...tl] =>
          if (a == b && b == p) {1 + checkTwos(p, tl)}
          else {checkTwos(p, [b, 0, 0, ...tl])}
        | [0, a, 0, b] =>
          if (a == b && b == p) {1}
          else {0}
        | [0, a, 0, b, c, ...tl] =>
          if (a == b && b == p && c != p) {1 + checkTwos(p, [c, ...tl])}
          else {checkTwos(p, [a, 0, b, c, ...tl])}
        | [a, 0, 0, b] =>
          if (a == b && b == p) {1}
          else {0}
        | [a, 0, 0, b, c, ...tl] =>
          if (a == b && b == p && c != p) {1 + checkTwos(p, [c, ...tl])}
          else {checkTwos(p, [0, 0, b, c, ...tl])}
        | [a, 0, b, 0, ...tl] =>
          if (a == b && b == p) {1 + checkTwos(p, tl)}
          else {checkTwos(p, [0, b, 0, ...tl])}
        | [_a, b, c, d, ...tl] =>
          checkTwos(p, [b,c,d, ...tl])
        | _ => 0
      }
// input: an int, p, and a list(int), column
// output: an int indicating how many potential four in a rows exist in the
// column for which exactly three of the four chips have been placed
and checkThrees: (int, list(int)) => int = (p, column) =>
      switch(column) {
        | [a, 0, b, c] =>
          if (a == b && b == c && c == p) {1}
          else {0}
        | [a, b, 0, c] =>
          if (a == b && b == c && c == p) {1}
          else {0}
        | [a, 0, b, c, ...tl] =>
          if (a == b && b == c && c == p) {1 + checkThrees(p, tl)}
          else {checkThrees(p, [0, b, c, ...tl])}
        | [a, b, 0, c, ...tl] =>
          if (a == b && b == c && c == p) {1 + checkThrees(p, tl)}
          else {checkThrees(p, [b, 0, c, ...tl])}
        | [0, a, b, c, ...tl] =>
          if (a == b && b == c && c == p) {1 + checkThrees(p, tl)}
          else {checkThrees(p, [a, b, c, ...tl])}
        | [a, b, c, 0, ...tl] =>
          if (a == b && b == c && c == p) {1 + checkThrees(p, tl)}
          else {checkThrees(p, [b, c, 0, ...tl])}
        | [_a, b, c, d, ...tl] =>
          checkThrees(p, [b,c,d, ...tl])
        | _ => 0
      }
// input: an int, p, and a list(list(int)), currentBoard, and a procedure
// output: the number of times the procedure is satisified for the given player
// across the entire game board
and orientationCounter: (int, (int, list(int)) => int, list(list(int))) =>
  int = (player, proc, currentBoard) =>
  switch(currentBoard) {
  | [] => 0
  | [hd, ...tl] => proc(player, hd) + orientationCounter(player, proc, tl)
  };
// input: an int, player, and a state, inState
// output: an int indicating the number of columns controlled by the player in
// the inState
let rec controlCounter: (int, state) => int = (player, inState) =>
  countTrue({let State(_, currentBoard) = inState
  wonControlBoard(player, legalMoves(inState), currentBoard)
  }) 
// input: an int, player, and a list(int), column
// output: a column where the player adds their token so that it is floating one
// slot above where it would land naturally, as in chipDrop
and columnControlDrop: (int, list(int)) => list(int) = (player, column) =>
  switch(column) {
    | [] => []
    | [0, 0] => [player, 0]
    | [hd, tl] => [hd, tl]
    | [0, 0, a, ...tl] =>
      if (a != 0) {[player, 0, a, ...tl]}
      else {[0, ...columnControlDrop(player, [0, a, ...tl])]}
    | [hd, ...tl] => [hd, ...tl]
  }
// input: an int, p, an int, column, and a list(list(int)), board
// output: a new board where a player adds their chip to the column as seen in
// columnControlDrop
and controlledColumnBoard: (int, int, list(list(int))) => list(list(int)) =
      (player, column, board) =>
        switch(board) {
        | [] => failwith("No Columns Available")
        | [hd, ...tl] when column<=1 => [columnControlDrop(player, hd), ...tl]
        | [hd, ...tl] => [hd, ...controlledColumnBoard(player, column-1, tl)]
        }
// input: an int, player, a list(move), legalMoves, and list(list(int)),
// currentBoard
// output: a list of boards where the given player adds a columnControl drop to
// each column 
and listControlBoard: (int, list(move), list(list(int))) =>
  list(list(list(int))) = (player, legalMoves, currentBoard) =>
  switch(legalMoves) {
    | [] => []
    | [Column(a), ...tl] =>
      [controlledColumnBoard(player, a, currentBoard),
        ...listControlBoard(player, tl, currentBoard)]
  }
// input: an int, player, a list(move), legalMoves, and list(list(int)),
// currentBoard
// output: a list of booleans, indicating which of the moves results in a win,
// meaning the given column is controlled by the palyer
and wonControlBoard: (int, list(move), list(list(int))) => list(bool) =
  (player, legalMoves, currentBoard) =>
  List.map(checkWinner, listControlBoard(player, legalMoves, currentBoard))
// input: a list(bool), alob
// output: an int indicating the number of trues in alob
and countTrue: list(bool) => int = alob =>
  switch(alob) {
    | [] => 0
    | [true, ...tl] => 1 + countTrue(tl)
    | [false, ...tl] => countTrue(tl)
  }
// input: a state, inState
// output: a float value indicating how much the current state favors P1 vs P2
let estimateValue: state => float =
    inState =>
      switch (inState) {
      | State(Win(P1), _) => 100000.
      | State(Win(P2), _) => (-100000.)
      | State(Draw, _) => 0.
      | State(Ongoing(_player), currentBoard) => float_of_int(
        2*completeCounter(1, checkTwos, currentBoard)
          - 2*completeCounter(2, checkTwos, currentBoard)
        + 7*completeCounter(1, checkThrees, currentBoard)
          - 7*completeCounter(2, checkThrees, currentBoard)
        + 12*controlCounter(1, inState)
          - 12*controlCounter(2, inState)
        )
      };
/* estimateValue optimization against online AI*/
// 2x v12 @ 2-7-8
// cannot beat v13 at these settings
// 3x v13 @ 2-7-14
// 3-2 v14 @ 2-7-14 with depth tweak
// 4-0 v14 @ 2-7-12 with depth tweak
/*------------------------------------------------------------------------------
stringOfPlayer
------------------------------------------------------------------------------*/
// stringOfPlayer
// input: a whichPlayer
// output: a string representing the player value
let stringOfPlayer: whichPlayer => string = p =>
      switch (p) {
      | P1 => "P1"
      | P2 => "P2"
      };
/*------------------------------------------------------------------------------
stringOfState (must implement visualization of game board)
------------------------------------------------------------------------------*/
// colorizeToken
// input: an int from the game board
// output: a string that takes the int and turns it into a colored string
let colorizeToken : int => string = token =>
  switch(token) {
    | 0 => "0"
    | 1 => "\027[32m" ++ "1" ++ "\027[0m"
    | 2 => "\027[31m" ++ "2" ++ "\027[0m"
    | _ => failwith("invalid token")
  };
// rowToString
// input: a row of the game board as a list(int)
// output: a string representation of the row
let rowToString : list(int) => string = row =>
  String.concat(" | ", List.map(colorizeToken, row))
// stringOfState
// input: a state
// output: a string representing the state to the player
let stringOfState: state => string = inState =>
      switch (inState) {
      | State(Win(_), a) =>
        String.concat("\n", List.map(rowToString, transpose(a))) ++ "\n"
      | State(Ongoing(_), a) =>
        String.concat("\n", List.map(rowToString, transpose(a))) ++ "\n\n"
        ++ "Current Game Value: "
        ++ string_of_int(int_of_float(estimateValue(inState))) ++ "\n"
      | State(Draw, a) =>
        String.concat("\n", List.map(rowToString, transpose(a)))
        ++ "\n\n" ++
        "No legal moves remain"
      };   
/*------------------------------------------------------------------------------
stringOfMove
------------------------------------------------------------------------------*/
// input: a move
// output: a string representing the move
let stringOfMove: move => string =
    inMove => {
      let Column(col) = inMove;
      "places a chip in column #" ++ string_of_int(col) ++ ": \n"
      }
/*------------------------------------------------------------------------------
moveOfString
------------------------------------------------------------------------------*/
// input: a string and a state
// output: a move extracted from the player input
let moveOfString: (string, state) => move = (col, inState) =>
    {let State(_, currentBoard) = inState
    switch(int_of_string_opt(col)){
      | Some(a) =>
        if (List.mem(Column(a), legalMoves(inState)))
          {Column(int_of_string(col))}
        else
          {failwith(
            "\n" ++
            String.concat("\n", List.map(rowToString, transpose(currentBoard)))
            ++ "\n\n" ++
            "Illegal move - please choose a column with vacant spaces\n")}
      | None => failwith(
            "\n" ++
            String.concat("\n", List.map(rowToString, transpose(currentBoard)))
            ++ "\n\n" ++
            "Illegal move - your input must be an integer\n")
    }}
};
module MyGame : Game = Connect4;
open Connect4;
/*------------------------------------------------------------------------------
TEST CASES
------------------------------------------------------------------------------*/
/* note - extensive testing was performed in the functional use of this program,
in addition to the following checkExpect and checkError cases */

/* currentPlayer */
checkExpect(currentPlayer(State(Win(P1), [[1]])), P1, "currentPlayer: 1")
checkExpect(currentPlayer(State(Win(P2), [[0]])), P2, "currentPlayer: 2")
checkError(() => currentPlayer(State(Draw, [[0]])),
  "currentPlayer domain error")

/* otherPlayer */
checkExpect(otherPlayer(P1), P2, "otherPlayer: 1")
checkExpect(otherPlayer(P2), P1, "otherPlayer: 2")

/* initialStateHelper */
checkExpect(initialStateHelper(5, 0), [], "initialStateHelper: 1")
checkExpect(initialStateHelper([], 3), [[], [], []], "initialStateHelper: 2")

/* initialState */
checkExpect(initialState("1 1"), State(Ongoing(P1), [[0]]), "initialState: 1")
checkExpect(initialState("2 3"), State(Ongoing(P1), [[0, 0], [0, 0], [0, 0]]),
  "initialState: 2")

/* gameStatus */
checkExpect(gameStatus(State(Ongoing(P1), [[1]])), Ongoing(P1), "gameStatus: 1")

/* checkwinner */
checkExpect(checkWinner(
    [
    [0,0,2,2,2],
    [0,0,0,0,0],
    [0,0,0,0,1],
    [0,0,0,0,1],
    [0,0,0,0,1],
    ]
    ),
    false, "checkWinner: 1")
checkExpect(checkWinner(
    [
    [0,0,2,2,2],
    [0,0,0,0,1],
    [0,0,0,0,1],
    [0,0,0,0,1],
    [0,0,0,0,1],
    ]
    ),
    true, "checkWinner: 2")

/* checkVertical */
checkExpect(checkVertical(
    [
    [0,0,2,2,2],
    [0,0,0,0,1],
    [0,0,0,0,1],
    [0,0,0,0,1],
    [0,0,0,0,1],
    ]
    ),
    false, "checkVertical: 1")
checkExpect(checkVertical(
    [
    [0,2,2,2,2],
    [0,0,0,0,1],
    [0,0,0,0,1],
    [0,0,0,0,1],
    [0,0,0,0,1],
    ]
    ),
    true, "checkVertical: 2")

/* checkVertical */
checkExpect(checkVerticalHelper([0,0,2,2,2]), false, "checkVerticalHelper: 1")
checkExpect(checkVerticalHelper([0,2,2,2,2]), true, "checkVerticalHelper: 2")

/* transpose */
checkExpect(transpose(
    [
    [0,0,2],
    [0,1,0],
    [0,2,0],
    ]),
    [
    [0,0,0],
    [0,1,2],
    [2,0,0],
    ], "transpose: 1")
checkError(()=>transpose([[]]), "the board cannot be zero-dimensional")

/* diagonalize */
checkExpect(diagonalize(
    [
    [0,0,2],
    [0,1,0],
    [0,2,0],
    ]),
    [[2],[0,0],[0,1,0],[0,2],[0]], "diagonalize: 1")

/* diagonalizeHelper */
checkExpect(diagonalizeHelper(
    [
    [0,0,2],
    [0,1,0],
    [0,2,0],
    ]),
    [[0,1,0], [0,2], [0]], "diagonalizeHelper: 1")

/* getDiagonal */
checkExpect(getDiagonal(
    [
    [0,0,2],
    [0,1,0],
    [0,2,0],
    ]),
    [0,1,0], "getDiagonal: 1")

/* nextState */
    checkExpect(nextState(State(Ongoing(P1), [[0,0],[0,0]]), Column(1)),
      State(Ongoing(P2), [[0,1],[0,0]]), "nextState: 1")
    checkExpect(nextState(State(Ongoing(P1), [[0,1,2], [0,1,2]]), Column(2)),
      State(Ongoing(P2), [[0,1,2], [1,1,2]]), "nextState: 2")
    checkExpect(nextState(State(Ongoing(P2), [[0,1,2],[1,2,2],[0,2,1]]),
      Column(3)),
      State(Ongoing(P1), [[0,1,2],[1,2,2],[2,2,1]]), "nextState: 3")

/* chipDrop */
    checkExpect(chipDrop(1,1,[[0,0],[0,0]]),
      [[0,1],[0,0]], "chipDrop: 1")
    checkExpect(chipDrop(1,2,[[0,0],[0,0],[0,0]]),
      [[0,0],[0,1],[0,0]], "chipDrop: 2")
    checkExpect(chipDrop(1,3,[[0,1,2],[0,2,2],[0,0,1]]),
      [[0,1,2],[0,2,2],[0,1,1]], "chipDrop: 3")
    checkExpect(chipDrop(1,3,[[0,1,2],[0,2,2],[0,2,1]]),
      [[0,1,2],[0,2,2],[1,2,1]], "chipDrop: 4")
checkError(()=>chipDrop(1,2,[[],[]]), "column of 0 height")

/* chipDropHelper */
checkExpect(chipDropHelper(1,[0]), [1], "chipDropHelper: 1")
checkExpect(chipDropHelper(1,[0, 1]), [1, 1], "chipDropHelper: 2")
checkError(()=>chipDropHelper(1,[1,2]), "illegal move: column filled")

/* playerToChip */
checkExpect(playerToChip(P1), 1, "playerToChip: 1")

/* boardSpaceQ */
checkExpect(boardSpaceQ([[1,2],[1,1]]), false, "boardSpaceQ: 1")
checkExpect(boardSpaceQ([[0,2],[1,1]]), true, "boardSpaceQ: 2")
  
/* legalMoves */
    checkExpect(legalMoves(State(Ongoing(P1), [[0,0],[0,0]])),
      [Column(1), Column(2)], "legalMoves: 1")
    checkExpect(legalMoves(State(Ongoing(P1), [[2,1],[1,2],[2,1]])),
      [], "legalMoves: 2")
    checkExpect(legalMoves(State(Ongoing(P2), [[0,1,2],[1,2,2],[0,2,1]])),
      [Column(1), Column(3)], "legalMoves: 3");

/* legalMovesHelper */
checkExpect(legalMovesHelper([true, false, false], 1), [Column(1)],
  "legalMovesHelper: 1")
checkExpect(legalMovesHelper([false, false, false], 1), [],
  "legalMovesHelper: 2")

/* legalColumns */
checkExpect(legalColumns([[0,1],[1,1],[0,0]]), [true, false, true],
  "legalColumns: 1")

/* completeCounter */
checkExpect(
  completeCounter(2, checkTwos, [
    [0,0,2,2,2],
    [0,0,0,0,0],
    [0,0,0,0,1],
    [0,0,0,0,1],
    [0,0,0,0,1],
    ]),
    0,
    "completeCounter")

/* columnControlDrop */
checkExpect(columnControlDrop(2, [0,0,0,2,1,2]), [0,2,0,2,1,2],
"columnControlDrop: 1");
checkExpect(columnControlDrop(1, [0,2]), [0,2],
"columnControlDrop: 2");
checkExpect(columnControlDrop(1, [0, 0, 1, 2, 2]), [1, 0, 1, 2, 2],
"columnControlDrop: 3");
 
/* controlledColumnBoard */
checkExpect(controlledColumnBoard(2, 2, [[0,0,0,2,1,2],[0,0,0,2,1,2],
[0,0,0,2,1,2]]), [[0,0,0,2,1,2],[0,2,0,2,1,2],[0,0,0,2,1,2]],
"controlledColumnBoard: 1");
checkExpect(controlledColumnBoard(1, 1, [[0,0,0,1,1,2],[0,0,0,1,1,2],
[0,0,0,1,1,2]]), [[0,1,0,1,1,2],[0,0,0,1,1,2],[0,0,0,1,1,2]],
"controlledColumnBoard: 2");
 checkExpect(
  controlledColumnBoard(2, 2, [[0,0,0,2,1,2],[0,0,0,2,1,2],[0,0,0,2,1,2]]),
  [[0,0,0,2,1,2],[0,2,0,2,1,2],[0,0,0,2,1,2]],
  "controlledColumnBoard: 3")

/* listControlBoard */
checkExpect(listControlBoard(1, [Column(1), Column(2)], [[0,0,0,2,1,2],
[0,0,0,2,1,2],[0,0,0,2,1,2]]), [[[0,1,0,2,1,2],[0,0,0,2,1,2],[0,0,0,2,1,2]],
[[0,0,0,2,1,2],[0,1,0,2,1,2],[0,0,0,2,1,2]]], "listControlBoard: 1");
checkExpect(listControlBoard(2, [], [[0,0,0,2,1,2],[0,0,0,2,1,2],
[0,0,0,2,1,2]]), [], "listControlBoard: 2");
checkExpect(
  listControlBoard(1, [Column(1), Column(2)],
    [[0,0,0,2,1,2],[0,0,0,2,1,2],[0,0,0,2,1,2]]),
  [[[0,1,0,2,1,2],[0,0,0,2,1,2],[0,0,0,2,1,2]],
    [[0,0,0,2,1,2],[0,1,0,2,1,2],[0,0,0,2,1,2]]],
  "listControlBoard: 3") 

/* estimateValue */
checkExpect(estimateValue(State(Win(P1), [[0, 0, 2, 1], [0, 0, 2, 1],
[0, 0, 2, 1], [0, 0, 0, 1]])), 100000., "estimateValue: 1");
checkExpect(estimateValue(State(Draw, [[2, 1, 2, 1], [2, 1, 2, 1], [2, 1, 2, 1],
[2, 1, 2, 1]])), 0., "estimateValue: 2");
checkExpect(estimateValue(State(Ongoing(P1), [[0, 0, 2, 1], [0, 0, 2, 1],
[0, 0, 2, 1], [0, 0, 0, 0]])), -12., "estimateValue: 3");

/* colorizeToken */
checkExpect(colorizeToken(0), "0", "colorizeToken: 1");
checkExpect(colorizeToken(1), "\027[32m" ++ "1" ++ "\027[0m",
  "colorizeToken: 1");
checkExpect(colorizeToken(2), "\027[31m" ++ "2" ++ "\027[0m",
  "colorizeToken: 2");
checkError(() => colorizeToken(5), "invalid token");

/* stringOfPlayer */
checkExpect(stringOfPlayer(P1), "P1", "stringOfPlayer: 1");
checkExpect(stringOfPlayer(P2), "P2", "stringOfPlayer: 2");
 
/* stringOfMove */
checkExpect(stringOfMove(Column(1)),
"places a chip in column #" ++ "1" ++ ": \n",
  "stringOfMove: 1");
checkExpect(stringOfMove(Column(2)),
"places a chip in column #" ++ "2" ++ ": \n",
  "stringOfMove: 2");
checkExpect(stringOfMove(Column(3)),
"places a chip in column #" ++ "3" ++ ": \n",
  "stringOfMove: 3");
 
/*stringOfState */
checkExpect(stringOfState(State(Win(P1), [[0, 0, 2, 1], [0, 0, 2, 1],
[0, 0, 2, 1], [0, 0, 0, 1]])), String.concat("\n", List.map(rowToString,
transpose([[0, 0, 2, 1], [0, 0, 2, 1], [0, 0, 2, 1], [0, 0, 0, 1]]))) ++ "\n",
"stringOfState: 1");
checkExpect(stringOfState(State(Draw, [[0, 0, 2, 1], [0, 0, 2, 1],
[0, 0, 2, 1], [0, 0, 0, 1]])), String.concat("\n", List.map(rowToString,
transpose([[0, 0, 2, 1], [0, 0, 2, 1], [0, 0, 2, 1], [0, 0, 0, 1]])))
++ "\n\n" ++ "No legal moves remain", "stringOfState: 2");
 
/* moveOfString */
checkExpect(moveOfString("3", State(Ongoing(P1), [[0, 0, 1, 2], [0, 0, 1, 2],
[0, 0, 1, 2], [0, 0, 0, 0]],)), Column(3), "moveOfString: 3");
checkExpect(moveOfString("1", State(Ongoing(P2), [[0, 1, 2, 1], [0, 0, 1, 2],
[0, 0, 1, 2], [0, 0, 0, 2]],),), Column(1), "moveOfString: 1");
checkError(() => moveOfString("0", State(Ongoing(P1), [[1, 2, 1, 1],
[0, 0, 0, 0]])), "\n" ++ String.concat("\n", List.map(rowToString,
transpose([[1, 2, 1, 1], [0, 0, 0, 0]]))) ++ "\n\n" ++
"Illegal move - please choose a column with vacant spaces\n");
checkError( () => moveOfString("apple", State(Draw, [[1]])), "\n" ++
            String.concat("\n", List.map(rowToString, transpose([[1]])))
            ++ "\n\n" ++
            "Illegal move - your input must be an integer\n")
checkError( () => moveOfString("0", State(Draw, [[1]])), "\n" ++
            String.concat("\n", List.map(rowToString, transpose([[1]])))
            ++ "\n\n" ++
            "Illegal move - please choose a column with vacant spaces\n")