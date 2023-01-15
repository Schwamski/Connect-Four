// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Setup$Game = require("./Setup.bs.js");
var Connect4$Game = require("./Connect4.bs.js");

function AIPlayer(MyGame) {
  var miniMax = function (inState, depth) {
    var match = Curry._1(MyGame.gameStatus, inState);
    if (typeof match === "number") {
      return 0;
    } else if (match.TAG === /* Win */0) {
      if (match._0) {
        return -100000 - depth | 0;
      } else {
        return 100000 + depth | 0;
      }
    } else if (match._0) {
      if (depth !== 0) {
        return chooseMin(List.map((function (x) {
                          return miniMax(x, depth - 1 | 0);
                        }), List.map((function (x) {
                              return Curry._2(MyGame.nextState, inState, x);
                            }), Curry._1(MyGame.legalMoves, inState))));
      } else {
        return Curry._1(MyGame.estimateValue, inState);
      }
    } else if (depth !== 0) {
      return chooseMax(List.map((function (x) {
                        return miniMax(x, depth - 1 | 0);
                      }), List.map((function (x) {
                            return Curry._2(MyGame.nextState, inState, x);
                          }), Curry._1(MyGame.legalMoves, inState))));
    } else {
      return Curry._1(MyGame.estimateValue, inState);
    }
  };
  var chooseMax = function (_alof) {
    while(true) {
      var alof = _alof;
      if (!alof) {
        return Pervasives.failwith("no options");
      }
      var match = alof.tl;
      var hd = alof.hd;
      if (!match) {
        return hd;
      }
      var tl = match.tl;
      var b = match.hd;
      if (hd > b) {
        _alof = {
          hd: hd,
          tl: tl
        };
        continue ;
      }
      _alof = {
        hd: b,
        tl: tl
      };
      continue ;
    };
  };
  var chooseMin = function (_alof) {
    while(true) {
      var alof = _alof;
      if (!alof) {
        return Pervasives.failwith("no options");
      }
      var match = alof.tl;
      var hd = alof.hd;
      if (!match) {
        return hd;
      }
      var tl = match.tl;
      var b = match.hd;
      if (hd < b) {
        _alof = {
          hd: hd,
          tl: tl
        };
        continue ;
      }
      _alof = {
        hd: b,
        tl: tl
      };
      continue ;
    };
  };
  var p2optimalMoveHelper = function (_legalMoves, inState, _leadingMove, _leadingValue) {
    while(true) {
      var leadingValue = _leadingValue;
      var leadingMove = _leadingMove;
      var legalMoves = _legalMoves;
      if (!legalMoves) {
        return Pervasives.failwith("no legal moves remain");
      }
      var tl = legalMoves.tl;
      var hd = legalMoves.hd;
      if (tl) {
        var hdVal = miniMax(Curry._2(MyGame.nextState, inState, hd), 4);
        if (hdVal < leadingValue) {
          _leadingValue = hdVal;
          _leadingMove = hd;
          _legalMoves = tl;
          continue ;
        }
        _legalMoves = tl;
        continue ;
      }
      var hdVal$1 = miniMax(Curry._2(MyGame.nextState, inState, hd), 4);
      if (hdVal$1 < leadingValue) {
        return [
                hd,
                hdVal$1
              ];
      } else {
        return [
                leadingMove,
                leadingValue
              ];
      }
    };
  };
  var p1optimalMoveHelper = function (_legalMoves, inState, _leadingMove, _leadingValue) {
    while(true) {
      var leadingValue = _leadingValue;
      var leadingMove = _leadingMove;
      var legalMoves = _legalMoves;
      if (!legalMoves) {
        return Pervasives.failwith("no legal moves remain");
      }
      var tl = legalMoves.tl;
      var hd = legalMoves.hd;
      if (tl) {
        var hdVal = miniMax(Curry._2(MyGame.nextState, inState, hd), 4);
        if (hdVal > leadingValue) {
          _leadingValue = hdVal;
          _leadingMove = hd;
          _legalMoves = tl;
          continue ;
        }
        _legalMoves = tl;
        continue ;
      }
      var hdVal$1 = miniMax(Curry._2(MyGame.nextState, inState, hd), 4);
      if (hdVal$1 > leadingValue) {
        return [
                hd,
                hdVal$1
              ];
      } else {
        return [
                leadingMove,
                leadingValue
              ];
      }
    };
  };
  var optimalMove = function (legalMoves, inState) {
    var match = Curry._1(MyGame.gameStatus, inState);
    if (typeof match === "number" || match.TAG === /* Win */0) {
      return Pervasives.failwith("optimalMove domain error");
    } else if (match._0) {
      return p2optimalMoveHelper(legalMoves, inState, List.hd(legalMoves), 200000)[0];
    } else {
      return p1optimalMoveHelper(legalMoves, inState, List.hd(legalMoves), -200000)[0];
    }
  };
  var nextMove = function (s) {
    return optimalMove(Curry._1(MyGame.legalMoves, s), s);
  };
  return {
          PlayerGame: MyGame,
          miniMax: miniMax,
          chooseMax: chooseMax,
          chooseMin: chooseMin,
          optimalMove: optimalMove,
          p1optimalMoveHelper: p1optimalMoveHelper,
          p2optimalMoveHelper: p2optimalMoveHelper,
          nextMove: nextMove,
          playerName: "Holy Trinity"
        };
}

var MyGame_stringOfPlayer = Connect4$Game.Connect4.stringOfPlayer;

var MyGame_stringOfState = Connect4$Game.Connect4.stringOfState;

var MyGame_stringOfMove = Connect4$Game.Connect4.stringOfMove;

var MyGame_initialState = Connect4$Game.Connect4.initialState;

var MyGame_legalMoves = Connect4$Game.Connect4.legalMoves;

var MyGame_gameStatus = Connect4$Game.Connect4.gameStatus;

var MyGame_nextState = Connect4$Game.Connect4.nextState;

var MyGame_moveOfString = Connect4$Game.Connect4.moveOfString;

var MyGame_estimateValue = Connect4$Game.Connect4.estimateValue;

var MyGame = {
  stringOfPlayer: MyGame_stringOfPlayer,
  stringOfState: MyGame_stringOfState,
  stringOfMove: MyGame_stringOfMove,
  initialState: MyGame_initialState,
  legalMoves: MyGame_legalMoves,
  gameStatus: MyGame_gameStatus,
  nextState: MyGame_nextState,
  moveOfString: MyGame_moveOfString,
  estimateValue: MyGame_estimateValue
};

function miniMax(inState, depth) {
  var match = Curry._1(Connect4$Game.Connect4.gameStatus, inState);
  if (typeof match === "number") {
    return 0;
  } else if (match.TAG === /* Win */0) {
    if (match._0) {
      return -100000 - depth | 0;
    } else {
      return 100000 + depth | 0;
    }
  } else if (match._0) {
    if (depth !== 0) {
      return chooseMin(List.map((function (x) {
                        return miniMax(x, depth - 1 | 0);
                      }), List.map((function (x) {
                            return Curry._2(Connect4$Game.Connect4.nextState, inState, x);
                          }), Curry._1(Connect4$Game.Connect4.legalMoves, inState))));
    } else {
      return Curry._1(Connect4$Game.Connect4.estimateValue, inState);
    }
  } else if (depth !== 0) {
    return chooseMax(List.map((function (x) {
                      return miniMax(x, depth - 1 | 0);
                    }), List.map((function (x) {
                          return Curry._2(Connect4$Game.Connect4.nextState, inState, x);
                        }), Curry._1(Connect4$Game.Connect4.legalMoves, inState))));
  } else {
    return Curry._1(Connect4$Game.Connect4.estimateValue, inState);
  }
}

function chooseMax(_alof) {
  while(true) {
    var alof = _alof;
    if (!alof) {
      return Pervasives.failwith("no options");
    }
    var match = alof.tl;
    var hd = alof.hd;
    if (!match) {
      return hd;
    }
    var tl = match.tl;
    var b = match.hd;
    if (hd > b) {
      _alof = {
        hd: hd,
        tl: tl
      };
      continue ;
    }
    _alof = {
      hd: b,
      tl: tl
    };
    continue ;
  };
}

function chooseMin(_alof) {
  while(true) {
    var alof = _alof;
    if (!alof) {
      return Pervasives.failwith("no options");
    }
    var match = alof.tl;
    var hd = alof.hd;
    if (!match) {
      return hd;
    }
    var tl = match.tl;
    var b = match.hd;
    if (hd < b) {
      _alof = {
        hd: hd,
        tl: tl
      };
      continue ;
    }
    _alof = {
      hd: b,
      tl: tl
    };
    continue ;
  };
}

function p2optimalMoveHelper(_legalMoves, inState, _leadingMove, _leadingValue) {
  while(true) {
    var leadingValue = _leadingValue;
    var leadingMove = _leadingMove;
    var legalMoves = _legalMoves;
    if (!legalMoves) {
      return Pervasives.failwith("no legal moves remain");
    }
    var tl = legalMoves.tl;
    var hd = legalMoves.hd;
    if (tl) {
      var hdVal = miniMax(Curry._2(Connect4$Game.Connect4.nextState, inState, hd), 4);
      if (hdVal < leadingValue) {
        _leadingValue = hdVal;
        _leadingMove = hd;
        _legalMoves = tl;
        continue ;
      }
      _legalMoves = tl;
      continue ;
    }
    var hdVal$1 = miniMax(Curry._2(Connect4$Game.Connect4.nextState, inState, hd), 4);
    if (hdVal$1 < leadingValue) {
      return [
              hd,
              hdVal$1
            ];
    } else {
      return [
              leadingMove,
              leadingValue
            ];
    }
  };
}

function p1optimalMoveHelper(_legalMoves, inState, _leadingMove, _leadingValue) {
  while(true) {
    var leadingValue = _leadingValue;
    var leadingMove = _leadingMove;
    var legalMoves = _legalMoves;
    if (!legalMoves) {
      return Pervasives.failwith("no legal moves remain");
    }
    var tl = legalMoves.tl;
    var hd = legalMoves.hd;
    if (tl) {
      var hdVal = miniMax(Curry._2(Connect4$Game.Connect4.nextState, inState, hd), 4);
      if (hdVal > leadingValue) {
        _leadingValue = hdVal;
        _leadingMove = hd;
        _legalMoves = tl;
        continue ;
      }
      _legalMoves = tl;
      continue ;
    }
    var hdVal$1 = miniMax(Curry._2(Connect4$Game.Connect4.nextState, inState, hd), 4);
    if (hdVal$1 > leadingValue) {
      return [
              hd,
              hdVal$1
            ];
    } else {
      return [
              leadingMove,
              leadingValue
            ];
    }
  };
}

function optimalMove(legalMoves, inState) {
  var match = Curry._1(Connect4$Game.Connect4.gameStatus, inState);
  if (typeof match === "number" || match.TAG === /* Win */0) {
    return Pervasives.failwith("optimalMove domain error");
  } else if (match._0) {
    return p2optimalMoveHelper(legalMoves, inState, List.hd(legalMoves), 200000)[0];
  } else {
    return p1optimalMoveHelper(legalMoves, inState, List.hd(legalMoves), -200000)[0];
  }
}

function nextMove(s) {
  return optimalMove(Curry._1(Connect4$Game.Connect4.legalMoves, s), s);
}

var playerName = "Holy Trinity";

var TestAIPlayer = {
  PlayerGame: MyGame,
  miniMax: miniMax,
  chooseMax: chooseMax,
  chooseMin: chooseMin,
  optimalMove: optimalMove,
  p1optimalMoveHelper: p1optimalMoveHelper,
  p2optimalMoveHelper: p2optimalMoveHelper,
  nextMove: nextMove,
  playerName: playerName
};

Setup$Game.checkExpect(chooseMax({
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: {
                  hd: 5,
                  tl: /* [] */0
                }
              }
            }
          }
        }), 5, "chooseMax: 1");

Setup$Game.checkExpect(chooseMax({
          hd: 1,
          tl: {
            hd: 1,
            tl: {
              hd: 1,
              tl: {
                hd: 1,
                tl: /* [] */0
              }
            }
          }
        }), 1, "chooseMax: 2");

Setup$Game.checkExpect(chooseMin({
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: {
                  hd: 5,
                  tl: /* [] */0
                }
              }
            }
          }
        }), 1, "chooseMin: 1");

Setup$Game.checkExpect(chooseMax({
          hd: 1,
          tl: {
            hd: 1,
            tl: {
              hd: 1,
              tl: {
                hd: 1,
                tl: /* [] */0
              }
            }
          }
        }), 1, "chooseMin: 2");

Setup$Game.checkError((function (param) {
        return Pervasives.failwith("no options");
      }), "no options");

Setup$Game.checkError((function (param) {
        return Pervasives.failwith("no options");
      }), "no options");

var adjustDepth = 4;

var TestGame;

var MyAIPlayer = {
  PlayerGame: MyGame,
  nextMove: nextMove,
  playerName: playerName
};

exports.adjustDepth = adjustDepth;
exports.AIPlayer = AIPlayer;
exports.TestGame = TestGame;
exports.TestAIPlayer = TestAIPlayer;
exports.MyAIPlayer = MyAIPlayer;
/*  Not a pure module */
