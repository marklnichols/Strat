import * as $ from "jquery";

//function flash (anId){
//    $('#'+anId).addClass("processing");
//    
//    setTimeout( function(){
//        $('#'+anId).removeClass("processing");
//    }, 1000);	// Timeout must be the same length as the CSS3 transition or longer (or you'll mess up the transition)
//}

class Game {
    constructor (public selections: Loc[], public validMoves: Moves, public latestMove: Move) {
        this.selections = selections;
        this.validMoves = validMoves;
        this.latestMove = latestMove
    }
}

class Loc {
    constructor(public col: string, public row: number) {}
}

class Moves {
    constructor(public moves: Move[]) {}
}

class Move {
    constructor(public locs: Loc[]) {}
}

class Square {
    constructor(public loc: Loc, public pieceType: number, public color: number) {}
}

var game = new Game([], new Moves([]), new Move([]));

//column indexes 0-7 -> A-H, row indexes 1-8
function rowCol2Id(col: number, row: number) { 
    return String.fromCharCode(97 + col) + row.toString();
}

function locToId(aLoc: Loc) {
    var aCol = aLoc.col
    var aRow = aLoc.row
    var anId = aCol + aRow.toString()
    return anId
}

function idToRow(id: string) {
    return parseInt(id.charAt(1))
}

function idToCol(id: string) {
    return id.charAt(0)
}

function flashSelections (){
    for (var i = 0; i < game.selections.length; i++) { 
        var aLoc = game.selections[i]
        var anId = locToId(aLoc)
        $('#'+anId).addClass("processing");
    }
    setTimeout( function(){
        resetSelections()
    }, 1000);
}

function resetSelections() {
    for (var i = 0; i < game.selections.length; i++) { 
        var aLoc = game.selections[i]
        var anId = locToId(aLoc)
        $('#'+anId).removeClass("selected"); 
        $('#'+anId).removeClass("processing"); 
    }
    game.selections = new Array();
}
       
function selectSquare(id: string) { 
    $('#'+id).addClass("selected"); 
}

function pushLocation(id: string) {
    var c = idToCol(id)
    var r = idToRow(id)
    var new_loc = new Loc(c, r)
    
    if ( !(isDuplicate(new_loc)) ) {
        game.selections.push(new_loc)
    }
}

function isDuplicate(new_loc: Loc) {
    var len = game.selections.length
    if (len == 0) {
        return false
    }
    var top = game.selections[len-1]
    if (JSON.stringify(top) === JSON.stringify(new_loc)) {
        return true
    } else {
        return false
    }
}

function selectSquareDbl(id: string) { 
    $('#'+id).addClass("dblclicked"); 
    pushLocation(id)
}

function submitMove(id: string) {
    var new_move = new Move(game.selections)
    if (checkValidMove(new_move)) {
        var json = JSON.stringify(new_move);
        flashSelections()
        $.ajax ({url: "http://localhost:3000/playerMove", method: "post", data: json, success: function(result) {
            setValidMoves(result.legalMoves);
            setLatestMove(result.latestMove)
            updateGameBoard(result.board);
        }})
    } else {
        alert ("Invalid move.");
        resetSelections()
   }
}

function checkValidMove(new_move: Move) {
    var valid = game.validMoves.moves;
    for (var i = 0; i < valid.length; i++) { 
        var move = valid[i]
        if (compareMoves(move, new_move) == true) {
            return true
        }
    }
    return false
}

function compareMoves(m1: Move, m2: Move) {
    var l1 = m1.locs
    var l2 = m2.locs
    
    if (l1.length != l2.length) {
        return false
    }
    for (var j = 0; j < l1.length; j++) {
        if (compareLocs(l1[j], l2[j]) == false) {
            return false
        }
    }
    return true
 }
 
 function compareLocs(loc1: Loc, loc2: Loc) {
    if (loc1.col.toUpperCase() == loc2.col.toUpperCase() && loc1.row == loc2.row) {
        return true
    } else {
        return false
    }
 }    

function onClick(event: Event) {
    pushLocation(this.id)
    selectSquare(this.id);
}

function onDblClick(event: Event) { 
    submitMove(this.id)
}

$(document).keydown(function(e: KeyboardEvent) {
  if(e.which == 27) {
    //alert ("You pressed the Escape key!");
    resetSelections()
  }
});

$(document).ready(function() { 
    // attach mouse click handler to each div sqare 
    for (var i = 0; i < 8; i++) { 
        for (var j = 1; j < 9; j++) { 
            //build strings #a1 - #h8
            var id = rowCol2Id(i, j); 
            $('#'+id).click(onClick);
            $('#'+id).dblclick(onDblClick);
        }
    }
    game.selections = new Array();
    //get pieces...
    $.ajax ({url: "http://localhost:3000/new", success: function(result) {
        setValidMoves(result.legalMoves)
        setLatestMove(result.latestMove)
        updateGameBoard(result.board);
    }})
});

function setValidMoves(moves: Moves) {
    game.validMoves = moves;
}

function setLatestMove(move: Move) {
    game.latestMove = move;
}

function updateGameBoard(squares: Square[]) {
    var whitePieceChar = '\u26c0'
    var blackPieceChar = '\u26c2'
    var whitePieceKing = '\u26c1'
    var blackPieceKing = '\u26c3'
    
    // clear the board
    for (var i = 0; i < 8; i++) {
        for (var j = 1; j < 9; j++) {
            var id = rowCol2Id(i, j);
            $('#'+id).text("");
        }
    }   
    // set the pieces
    for (var i = 0; i < squares.length; i++) {
        var e = $('#'+squares[i].loc.col.toLowerCase()+squares[i].loc.row);
        if (squares[i].pieceType == 1) { //if regular piece
            $(e).text(squares[i].color === 1 ? whitePieceChar : blackPieceChar);
        } else { //else piece is a king
            $(e).text(squares[i].color === 1 ? whitePieceKing : blackPieceKing);
        }
    }
    //autoPlay()
}

//function autoPlay() {
    //$.ajax ({url: "http://localhost:3000/computerMove", success: function(result) {
    //    updateGameBoard(result);
    //}})
//}
