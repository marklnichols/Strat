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
