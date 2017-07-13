import * as $ from "jquery";

class Loc {
    constructor(public col: string, public row: number) {}
}

class Move {
    constructor(public locs: Loc[]) {}
}

class Moves {
    constructor(public moves: Move[]) {}
}

class Game {
    constructor (public selections: Loc[], public highlights: LocationClick[], public legalMoves: Moves,
                 public latestMove: Loc[]) {
        this.selections = selections;
        this.highlights = highlights;
        this.legalMoves = legalMoves;
        this.latestMove = latestMove;
    }
}

var game = new Game([],[], new Moves([]), []);

class Square {
    constructor(public loc: Loc, public pieceType: number, public color: number) {}
}

class Result {
    constructor(public msg: string, public prevBoard: Square[], public board: Square[], 
                public legalMoves: Moves, public latestMove: Loc[]) {}
}

var LocEnum = {
    INITIAL : 0,
    MULTI : 1,
    FINAL : 2,
    NONE : 3
}

class LocationClick {
    constructor(public loc: Loc, public locType : number) {}
}

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

function idToLoc(id: string): Loc {
    var row = idToRow(id);
    var col = idToCol(id);
    return new Loc(col, row);
}

function idToRow(id: string) {
    return parseInt(id.charAt(1))
}

function idToCol(id: string) {
    return id.charAt(0)
}

function addCSSClassToLoc(locs: Loc[], cssClass: string) {
      for (var i = 0; i < locs.length; i++) { 
        var aLoc = locs[i]
        var anId = locToId(aLoc)
        $('#'+anId).addClass(cssClass);
    } 
}

function rmClassesFromLocs(locs: Loc[]) {
    for (var i = 0; i < locs.length; i++) { 
        rmCSSClasses(locs[i]);
    } 
}

function rmClassesFromLCs(lcs: LocationClick[]) {
    for (var i = 0; i < lcs.length; i++) { 
        rmCSSClasses(lcs[i].loc);
    } 
}

function rmCSSClasses(loc: Loc) {
    var anId = locToId(loc);
    $('#'+anId).removeClass("selected"); 
    $('#'+anId).removeClass("computermove"); 
    $('#'+anId).removeClass("playermove"); 
    $('#'+anId).removeClass("initial"); 
    $('#'+anId).removeClass("multi"); 
    $('#'+anId).removeClass("final"); 
}

function addSelectedCss(id: string) { 
    $('#'+id).addClass("selected"); 
}

function addInitialCss(id: string) { 
    $('#'+id).addClass("initial"); 
}

function addMultiCss(id: string) { 
    $('#'+id).addClass("multi"); 
}

function addFinalCss(id: string) { 
    $('#'+id).addClass("final"); 
}

function submitMove(id: string) {
    var locs = game.selections;
    var new_move = new Move(locs);
    if (checkLegalMove(new_move)) {
        clearHighlights();
        var json = JSON.stringify(new_move);
        addCSSClassToLoc(locs, "playermove")
        $.ajax ({url: "http://localhost:3000/playerMove", method: "post", data: json, success: function(result) {
            setLegalMoves(result.legalMoves);
            setLatestMove(result.latestMove)
            clearSelected();
            updateGameBoard(result.prevBoard)
            addCSSClassToLoc(result.latestMove.locs, "computermove")
            setTimeout( function(){
                updateGameBoard(result.board)
                rmClassesFromLocs(result.latestMove.locs);
                var inits = findInitials(result.legalMoves)
                clearHighlights();
                addHighlights(inits);
              }, 2000); 
        }})
    } else {
        alert ("Invalid move: " + moveToStr(new_move));
        clearSelected();
        var inits = findInitials(game.legalMoves)
        clearHighlights();
        addHighlights(inits);
   }
}

function moveToStr(move: Move) {
    var moveStr = ""
    for (var i: number = 0; i < move.locs.length; i++ ) {
        var loc = move.locs[i]
        moveStr = moveStr + locToId(loc) + " "
    }
    return moveStr;
}

function checkLegalMove(new_move: Move) {
    var legal = game.legalMoves.moves;
    for (var i = 0; i < legal.length; i++) { 
        var move = legal[i]
        if (compareMoves(move, new_move) == true) {
            return true
        }
    }
    return false
}

function compareMoves(m1: Move, m2: Move) {
    if (m1.locs.length != m2.locs.length)
        return false

    for (var j = 0; j < m1.locs.length; j++) {
        if (compareLocs(m1.locs[j], m2.locs[j]) == false)
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

 function findLocInLCs(loc: Loc, lcs: LocationClick[]): number {
    var len = lcs.length;
    //check: is this loc highlighted as a "clickable" square?
    for (var i=0; i < len; i++) {
        if (compareLocs(loc, lcs[i].loc)) 
            return lcs[i].locType;
    }
    return LocEnum.NONE;
 }

function onClick(event: Event) {
    var loc = idToLoc(this.id);
    var highs = game.highlights;
    var theType = findLocInLCs(loc, highs);
    if (theType == LocEnum.INITIAL) { 
        clearSelected();
        clearHighlights();
        addSelected(loc);
        var conts = findContinues(game.legalMoves, loc);
        addHighlights(conts);
    } else if (theType == LocEnum.MULTI) {
        clearHighlights();
        addSelected(loc);
        var conts = findContinues(game.legalMoves, loc);
        addHighlights(conts);

    } else if (theType == LocEnum.FINAL) {
        clearHighlights();
        addSelected(loc);
        submitMove(this.id);

    } else {    //LocEnum.None -- not a "clickable" square
        clearSelected();
        clearHighlights();
        var inits = findInitials(game.legalMoves);
        addHighlights(inits);

    }
}

function addHighlights(lcs: LocationClick[]) {
    for (var i=0; i<lcs.length; i++) {
        game.highlights.push(lcs[i])
    }
    highlightLocations(lcs)
}

function clearHighlights() {
    var oldHs = game.highlights;
    rmClassesFromLCs(oldHs);
    game.highlights = [];
}

function addSelected(loc: Loc) {
    game.selections.push(loc);
    rmCSSClasses(loc);
    addSelectedCss(locToId(loc));
}

function pushUniqueLC(theLc: LocationClick, lcs: LocationClick[]): LocationClick[] {
    var len = lcs.length;
    for (var i=0; i < len; i++) {
        var lc = lcs[i];
        if (compareLocs(lc.loc, theLc.loc))
            if (lc.locType == theLc.locType)
                return lcs;
    }
    lcs.push(theLc);
    return lcs;
}

function clearSelected() {
    var oldSel = game.selections;
    rmClassesFromLocs(oldSel);
    game.selections = [];
}

$(document).keydown(function(e: KeyboardEvent) {
  if(e.which == 27) {
    //alert ("You pressed the Escape key!");
    clearSelected();
    var inits = findInitials(game.legalMoves)
    clearHighlights();
    addHighlights(inits); 
  }
});

var whitePiece: string = "checker_1_plain_48.png"; 
var blackPiece: string = "checker_2_plain_48.png";
var whiteKing: string = "checker_1_king_48.png"; 
var blackKing: string = "checker_2_king_48.png";
var noPiece: string = "no_image_48.png";

function buildTag(imgId: string, imgName: string) {
    return "<div class='img-wrapper'><img class='piece' id=" + imgId + " src=" +  imgName + "></div>"
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

$(document).ready(function() { 
    $("#restart").click(function(){
        newGame();
    }); 
    // attach mouse click handler to each div sqare
    // also, add html tag holding each piece image 
    for (var j=1; j<9; j++) {
        for (var i=0; i<8; i+=2) {
            var iIndex = i + ((j+1) % 2)
            //build strings #a1 - #h8
            var id = rowCol2Id(iIndex, j);
            var imgId = imageId(iIndex, j) 
            $('#'+id).click(onClick);
            var tag = buildTag(imgId, noPiece);
            $('#'+id).html(tag);   
        }
    }
    newGame();
});

function newGame() {
    $.ajax ({url: "http://localhost:3000/new", success: function(result) {
        setLegalMoves(result.legalMoves)
        setLatestMove(result.latestMove)
        updateGameBoard(result.board);
        clearSelected();
        var inits = findInitials(result.legalMoves)
        clearHighlights();
        addHighlights(inits);
    }}) 
}

function clearPieces() {
    for (var j=1; j<9; j++) {
        for (var i=0; i<8; i+=2) {
            var iIndex = i + ((j+1) % 2)
            var imgId = imageId(iIndex, j) 
            $('#'+imgId).attr("src", noPiece);
        }
    }
}

function setLegalMoves(moves: Moves) {
    game.legalMoves = moves;
}

function highlightLocations(lcs: LocationClick[]) {
    for (var i = 0; i < lcs.length; i++) {
        var theLC = lcs[i];
        var theId = locToId(theLC.loc);
        var theType = theLC.locType;
        if (theType == LocEnum.INITIAL)
            addInitialCss (theId);
        else if (theType == LocEnum.MULTI)
            addMultiCss(theId);
        else if (theType == LocEnum.FINAL)
            addFinalCss(theId);
    }
}

//find all locations where a new move can be initiated
function findInitials(ms: Moves): LocationClick[] {
    var legalMoves = ms.moves;
    var theLocs: LocationClick[] = [];
    for (var i=0; i< legalMoves.length; i++) {
        var locs = legalMoves[i].locs;
        var theLocClick = new LocationClick(locs[0], LocEnum.INITIAL)
        theLocs = pushUniqueLC(theLocClick, theLocs); 
    }
    return theLocs;
}

//find all locations that are legal move continuations from the given clicked loc
function findContinues(ms: Moves, loc: Loc): LocationClick[] {
    var legalMoves = ms.moves;
    var theLCs: LocationClick[] = [];
    var nLocs: number = legalMoves.length;
    var str: string = "nLocs is: " + nLocs.toString();
    $("#para1").html(str);
    //alert (str);
    for (var i=0; i < nLocs; i++) {
        var move = legalMoves[i];
        var index = findLocInMove(loc, move);
        if (index != -1) {
            var len = move.locs.length;
            if (index < len-2) { //if the next loc is not the last loc in the move
                var multi = new LocationClick(move.locs[index+1], LocEnum.MULTI);
                theLCs = pushUniqueLC(multi, theLCs); 
            } else if (index == len-2) { //if the last loc in the move
                var final = new LocationClick(move.locs[index+1], LocEnum.FINAL);
                theLCs = pushUniqueLC(final, theLCs);
            }
        }
    }
    return theLCs;
}

function findLocInMove(theLoc: Loc, theMove: Move): number {
    for (var i=0; i < theMove.locs.length; i++) {
        if (compareLocs(theLoc, theMove.locs[i]))
            return i;    
    }
    return -1;
}

function setLatestMove(move: Loc[]) {
    game.latestMove = move;
}

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
                imgName = whiteKing;
            else 
                imgName = blackKing;
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
