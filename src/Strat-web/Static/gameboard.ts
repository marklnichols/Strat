import * as $ from "jquery";

class Game {
    constructor (public selections: Loc[], public flashing: Loc[], public validMoves: Moves, 
                 public latestMove: Move) {
        this.selections = selections;
        this.flashing = flashing;
        this.validMoves = validMoves;
        this.latestMove = latestMove;
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

class Result {
    constructor(public msg: string, public prevBoard: Square[], public board: Square[], 
                public legalMoves: Move[], public latestMove: Move) {}
}

var game = new Game([], [], new Moves([]), new Move([]));

//column indexes 0-7 -> A-H, row indexes 1-8
function rowCol2Id(col: number, row: number) { 
    return String.fromCharCode(97 + col) + row.toString();
}

function locToId(aLoc: Loc) {
    var aCol = aLoc.col
    var aRow = aLoc.row
    var anId = aCol.toLowerCase() + aRow.toString();
    return anId
}

function idToRow(id: string) {
    return parseInt(id.charAt(1))
}

function idToCol(id: string) {
    return id.charAt(0)
}

function addCSSClass(locs: Loc[], cssClass: string) {
      for (var i = 0; i < locs.length; i++) { 
        var aLoc = locs[i]
        var anId = locToId(aLoc)
        $('#'+anId).addClass(cssClass);
    } 
}

function rmCSSClasses(locs: Loc[]) {
      for (var i = 0; i < locs.length; i++) { 
        var aLoc = locs[i]
        var anId = locToId(aLoc)
        $('#'+anId).removeClass("selected"); 
        $('#'+anId).removeClass("computermove"); 
        $('#'+anId).removeClass("playermove"); 
    } 
}

function resetSelections() {
    for (var i = 0; i < game.selections.length; i++) { 
        var aLoc = game.selections[i]
        var anId = locToId(aLoc)
        $('#'+anId).removeClass("selected"); 
        $('#'+anId).removeClass("playermove"); 
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
        addCSSClass(new_move.locs, "playermove")
        $.ajax ({url: "http://localhost:3000/playerMove", method: "post", data: json, success: function(result) {
            setValidMoves(result.legalMoves);
            setLatestMove(result.latestMove)
            rmCSSClasses(new_move.locs)
            game.selections = new Array();
            updateGameBoard(result.prevBoard)
            addCSSClass(result.latestMove.locs, "computermove")
            setTimeout( function(){
                rmCSSClasses(result.latestMove.locs)
                updateGameBoard(result.board)
              }, 2000); 
        }})
    } else {
        alert ("Invalid move: " + moveToStr(new_move));
        resetSelections()
   }
}

function moveToStr(move: Move) {
    var locs = move.locs
    var moveStr = ""
    for (var i: number = 0; i < locs.length; i++ ) {
        var loc = locs[i]
        moveStr = moveStr + locToId(loc) + " "
    }
    return moveStr;
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
    if (l1.length != l2.length)
        return false

    for (var j = 0; j < l1.length; j++) {
        if (compareLocs(l1[j], l2[j]) == false)
            return false
    }
    return true
 }
 
 function compareLocs(loc1: Loc, loc2: Loc) {
    if (loc1.col.toLowerCase() == loc2.col.toLowerCase() && loc1.row == loc2.row) 
        return true
     else 
        return false
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

var whitePiece: string = "checker_1_plain_48.png"; 
var blackPiece: string = "checker_2_plain_48.png";
var noPiece: string = "no_image_48.png";

function imageTag(isWhite: Boolean, row: number , col: number) {
    var imgName: string;
    var imgId = imageId(col, row);
    if (isWhite) 
        imgName = whitePiece;
    else 
        imgName = blackPiece;
}

function buildTag(imgId: string, imgName: string) {
    return "<p style='text-align:center'><img id=" + imgId + " src=" + 
            imgName + " style='img.resize'" +  " display: block margin-left: auto margin-right: auto></p>"
            //imgName + " style='img.resize'" +  " display: block margin-left: 2px margin-right: 2px></p>"
}

var imgPrefix: string = "img-";

function imageId(col: number, row: number) {
    var _id = rowCol2Id(col, row)
    return imgPrefix + _id;
}

function locToImgId(loc: Loc) {
    var _id = locToId(loc);
    return imgPrefix + _id;
}

function showRowColImage(col: number, row: number,) {
    var id: string;
    id = imageId(col, row);
    showImage(id);
}

function showImage(imageId: string) {
     $('#'+imageId).fadeIn( "slow", function() {
        // Animation complete.
  });
}

function hideRowColImage(col: number, row: number) {
    var id: string;
    id = imageId(col, row);
    hideImage(id);
}

function hideImage(imageId: string) {
  $('#'+imageId).fadeOut( "slow", function() {
        // Animation complete.
  });
}

$(document).ready(function() { 
    // attach mouse click handler to each div sqare
    // also, add html tag holding each piece image 
    for (var j=1; j<9; j++) {
        for (var i=0; i<8; i+=2) {
            var iIndex = i + ((j+1) % 2)
            //build strings #a1 - #h8
            var id = rowCol2Id(iIndex, j);
            var imgId = imageId(iIndex, j) 
            $('#'+id).click(onClick);
            $('#'+id).dblclick(onDblClick);
            var tag = buildTag(imgId, noPiece);
            $('#'+id).html(tag);   
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

function clearPieces() {
    for (var j=1; j<9; j++) {
        for (var i=0; i<8; i+=2) {
            var iIndex = i + ((j+1) % 2)
            var imgId = imageId(iIndex, j) 
            $('#'+imgId).attr("src", noPiece);
        }
    }
}

function setValidMoves(moves: Moves) {
    game.validMoves = moves;
}

function setLatestMove(move: Move) {
    game.latestMove = move;
}

/*
<div id="clickme">
  Click here
</div>
<img id="book" src="book.png" alt="" width="100" height="123">
 
With the element initially shown, we can hide it slowly:

$( "#clickme" ).click(function() {
  $( "#book" ).fadeOut( "slow", function() {
    // Animation complete.
  });
});
*/

function updateGameBoard(squares: Square[]) {
    // clear the board
    clearPieces()

    // set the pieces
    for (var i = 0; i < squares.length; i++) {
        var loc = squares[i].loc;
        var id = locToId(loc);
        var imgId: string = locToImgId(loc);
        var imgName: string;
        if (squares[i].pieceType == 1) { //if regular piece
            if (squares[i].color == 1) 
                imgName = whitePiece;
            else 
                imgName = blackPiece;
        } else { //king
             if (squares[i].color == 1) 
                imgName = whitePiece;
            else 
                imgName = blackPiece;
        }
        $('#'+imgId).attr("src", imgName);
    }
    //autoPlay()
}

//function autoPlay() {
    //$.ajax ({url: "http://localhost:3000/computerMove", success: function(result) {
    //    updateGameBoard(result);
    //}})
//}
