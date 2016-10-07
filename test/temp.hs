
let node = treeFromGridB blunderBoard0
let newTree = runReader (expandTree node) gameEnv
let resultM = runReader (best newTree (-1)) gameEnv

let result = fromJust resultM
let badMovesM = checkBlunders newTree (-1) (result^.moveScores)
let badMoves = runReader badMovesM gameEnv
