open! Setup;
open Game;

/*********************** README FOR TOURNAMENT  ********************************
IF AI IS RUNNING TOO SLOW, DECREASE adjustDepth FROM 4 TO 3
(optimal performance at 4, faster performance at 3)
*******************************************************************************/
let adjustDepth = 4;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame
/*------------------------------------------------------------------------------
miniMax
------------------------------------------------------------------------------*/
// input: a game state and an integer, depth
// output: a float value indicating the value of the game state as determined
// by the minimax algorithm
//
// Recurison Diagram #1:
// OI: (<inState>, 0)
//    RI: N/A
//    RO: N/A
// RO: estimateValue(<inState>)
//
// Recurison Diagram #2:
// OI: (<inState>, 2)
//    RI: [tuples of all states created from each legal move and depth-1]
//    RO: [a float value of all states in RI]
// RO: [the min/max float value of the RO depending on whose turn it is]
let rec miniMax: (PlayerGame.state, int) => float = (inState, depth) =>
      switch(PlayerGame.gameStatus(inState)) {
      | Win(P1) => float_of_int(100000 + depth)
      | Win(P2) => float_of_int(-100000 - depth)
      | Draw => 0.
      | Ongoing(P1) =>
        switch(depth) {
          | 0 => PlayerGame.estimateValue(inState)
          | _ => 
            chooseMax(List.map(x => miniMax(x, depth-1),
              List.map(x => PlayerGame.nextState(inState,x),
                PlayerGame.legalMoves(inState))))
        }
      | Ongoing(P2) =>
        switch(depth) {
          | 0 => PlayerGame.estimateValue(inState)
          | _ =>
            chooseMin(List.map(x => miniMax(x, depth-1),
              List.map(x => PlayerGame.nextState(inState,x),
                PlayerGame.legalMoves(inState))))
      }
  }
// chooseMax
// input: a list of floats
// output: the maximum float value
and chooseMax: list(float) => float = alof =>
  switch(alof) {
    | [] => failwith("no options")
    | [hd] => hd
    | [a, b, ...tl] =>
      if (a > b) {chooseMax([a, ...tl])}
      else {chooseMax([b, ...tl])}
  }
// chooseMin
// input: a list of floats
// output: the minimum float value
and chooseMin: list(float) => float = alof =>
  switch(alof) {
    | [] => failwith("no options")
    | [hd] => hd
    | [a, b, ...tl] =>
      if (a < b) {chooseMin([a, ...tl])}
      else {chooseMin([b, ...tl])}
  }
/*------------------------------------------------------------------------------
optimalMove
------------------------------------------------------------------------------*/
// input: a list of legal moves, legalMoves, and the game state, inState
// output: a single move which is the optimal move for the player to make
let rec optimalMove: (list(PlayerGame.move), PlayerGame.state) =>
  PlayerGame.move = (legalMoves, inState) =>
  switch(PlayerGame.gameStatus(inState)) {
    | Ongoing(P1) =>
      {let (topMove, _topValue) =
        p1optimalMoveHelper(legalMoves, inState, List.hd(legalMoves), -200000.)
      topMove}
    | Ongoing(P2) => 
      {let (topMove, _topValue) =
        p2optimalMoveHelper(legalMoves, inState, List.hd(legalMoves), 200000.)
      topMove}
    | _ => failwith("optimalMove domain error")
  }
// input: a list of moves, legalMoves; a state, inState; a move, leadingMove;
// and a float, leadingValue; (latter two for strengthened recursion)
// output: the move from legalMoves that returns the highest float value and a
// float representing the value of the move when miniMax is applied
and p1optimalMoveHelper:
  (list(PlayerGame.move), PlayerGame.state, PlayerGame.move, float) =>
  (PlayerGame.move, float) = (legalMoves, inState, leadingMove, leadingValue) =>
  switch(legalMoves) {
    | [] => failwith("no legal moves remain")
    | [hd] =>
      {let hdVal = miniMax(PlayerGame.nextState(inState,hd), adjustDepth)
      if (hdVal > leadingValue)
        {(hd, hdVal)}
      else
        {(leadingMove, leadingValue)}
      }
    | [hd, ...tl] =>
      {let hdVal = miniMax(PlayerGame.nextState(inState,hd), adjustDepth)
      if (hdVal > leadingValue)
        {p1optimalMoveHelper(tl, inState, hd, hdVal)}
      else
        {p1optimalMoveHelper(tl, inState, leadingMove, leadingValue)}
      }
  }
// input: a list of moves, legalMoves; a state, inState; a move, leadingMove;
// and a float, leadingValue; (latter two for strengthened recursion)
// output: the move from legalMoves that returns the lowest float value and a
// float representing the value of the move when miniMax is applied
and p2optimalMoveHelper:
  (list(PlayerGame.move), PlayerGame.state, PlayerGame.move, float) =>
  (PlayerGame.move, float) = (legalMoves, inState, leadingMove, leadingValue) =>
  switch(legalMoves) {
    | [] => failwith("no legal moves remain")
    | [hd] =>
      {let hdVal = miniMax(PlayerGame.nextState(inState,hd), adjustDepth)
      if (hdVal < leadingValue)
        {(hd, hdVal)}
      else
        {(leadingMove, leadingValue)}
      }
    | [hd, ...tl] =>
      {let hdVal = miniMax(PlayerGame.nextState(inState,hd), adjustDepth)
      if (hdVal < leadingValue)
        {p2optimalMoveHelper(tl, inState, hd, hdVal)}
      else
        {p2optimalMoveHelper(tl, inState, leadingMove, leadingValue)}
      }
  }
// input: a game state, s
// output: the optimal move for the AI to make given the game state
  let nextMove: PlayerGame.state => PlayerGame.move = s => 
    optimalMove(PlayerGame.legalMoves(s), s)
  let playerName = "Holy Trinity";
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer:Player = TestAIPlayer;
open TestAIPlayer;

// TEST CASES (excluding procedures that use Playergame.state in their I/O)
checkExpect(chooseMax([1.,2.,3.,4.,5.]), 5., "chooseMax: 1")
checkExpect(chooseMax([1.,1.,1.,1.,]), 1., "chooseMax: 2")
checkExpect(chooseMin([1.,2.,3.,4.,5.]), 1., "chooseMin: 1")
checkExpect(chooseMax([1.,1.,1.,1.,]), 1., "chooseMin: 2")
checkError(()=>chooseMax([]),"no options")
checkError(()=>chooseMin([]),"no options")

