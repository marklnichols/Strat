
let node = treeFromGridB blunderBoard0
let newTree = runReader (expandTree node) gameEnv
let resultM = runReader (best newTree (-1)) gameEnv
:force resultM

let result = fromJust resultM
let badMovesM = checkBlunders newTree (-1) (result^.moveScores)
let badMoves = runReader badMovesM gameEnv



let node = treeFromGridB evalBoard01

let vite = setColor (rootLabel node) 1
let vite' = getPieceLocs vite
let viteLocs = getNonKingLocs vite

let blek = setColor (rootLabel node) (-1)
let blek' = getPieceLocs blek
let blekLocs = getNonKingLocs blek

let g =  (rootLabel node) ^. (ckPosition . grid)

pieceProgress viteLocs 1
pieceProgress blekLocs (-1)

-----------------------------------------------------------------------------------------
-- Game where x ahead:
-----------------------------------------------------------------------------------------
--X's move
Computer's move:
 (m:B6-D8, s:Total 24 made up of mat<20> mob<5> home<0> prog<-1>)

H     -     -     -     o
G  -     x     o     -
F     -     -     -     x
E  -     o     o     -
D     -     -     -     x
C  x     -     x     -
B     -     -     -     -
A  -     -     -     x

   1  2  3  4  5  6  7  8
Current position score: Total 17 made up of mat<10> mob<2> home<0> prog<5>
X is ahead in mat, so
    current position score is correct
    and X's computer move score is correct

-------------------------------------------------------------------------------------

--O's move
Computer's move:
 (m:G5-F6, s:Total -22 made up of mat<20> mob<2> home<0> prog<0>)

    H     -     -     -     o
    G  -     x     -     -
    F     -     -     o     x
    E  -     o     o     -
    D     -     -     -     x
    C  x     -     x     -
    B     -     -     -     -
    A  -     -     -     x

       1  2  3  4  5  6  7  8
    Current position score: Total 18 made up of mat<10> mob<3> home<0> prog<5>
X is ahead in mat, so
    current pos score is correct
    O's computer move total should be positive too


-----------------------------------------------------------------------------------------
-- Game where O ahead:
-----------------------------------------------------------------------------------------
--Xs move
Computer's move:
 (m:F2-G1, s:Total -38 made up of mat<-30> mob<-8> home<0> prog<0>)

H     -     O     -     -
G  X     -     -     -
F     -     O     -     -
E  -     -     -     -
D     -     -     -     -
C  -     -     -     -
B     O     -     -     -
A  -     -     -     -

   1  2  3  4  5  6  7  8
Current position score: Total -38 made up of mat<-30> mob<-8> home<0> prog<0>
--O is ahead so
current position is correct
computer move score: correct

--O's move
Computer's move:
 (m:B2-C3, s:Total 40 made up of mat<-30> mob<-10> home<0> prog<0>)

H     -     O     -     -
G  X     -     -     -
F     -     O     -     -
E  -     -     -     -
D     -     -     -     -
C  -     O     -     -
B     -     -     -     -
A  -     -     -     -

   1  2  3  4  5  6  7  8
Current position score: Total -38 made up of mat<-30> mob<-8> home<0> prog<0>
--O is ahead so,
    current position is correct
    computer's move Total should be negative

IN ALL CASES, when X's turn, in the Computer's MOve section only, the total only needs to be flipped
--error is try for all the list of move choices as well -- the invididual scorecomponents are correct

from this:
putStrLn ("Current position score: " ++ show (getValue (rootLabel node)))
then the value stored on the node when it is created is correct

but the things returned back from best, worst, etc. are flipped
