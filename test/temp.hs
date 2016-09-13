{-- how indexes relate to board position (indexes in parens are not displayed):

   (41) (42) (43) (44) (45)

H|     37  38  39  40
G|   32  33  34  35  (36)
F|     28  29  30  31
E|   23  24  25  26  (27)
D|     19  20  21  22
C|   14  15  16  17  (18)
B|     10  11  12  13
A|   05  06  07  08  (09)

   (00) (01) (02) (03) (04)
     ---------------
     1 2 3 4 5 6 7 8
--}


pieceProgress :: V.Vector Int -> Int -> Int

import qualified Data.Vector.Unboxed as V
let vsFront = V.fromList [5, 11, 16, 20]
let vsBack = V.fromList [25, 29, 35, 37]

let f vs color = V.map (\x -> (labelIdx x + (color - 1) * 7 `div` 2)) vs

f vsFront 1
    [0,1,2,3]

f vsBack 1
    [4,5,6,7]

f vsFront (-1)
    [-7,-6,-5,-4]

f vsBack (-1)
    [-3,-2,-1,0]
