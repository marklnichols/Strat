kingProximity (nodeFromGridW board01) `shouldBe` 0
kingProximity (nodeFromGridW board11) `shouldBe` 2
kingProximity (nodeFromGridW board09b) `shouldBe` (-1)
kingProximity (nodeFromGridW blunderBoard0) `shouldBe` 2
kingProximity (nodeFromGridB blunderBoard0) `shouldBe` 2



let n11 = nodeFromGridW board11
kingProximity n11

let wAll   = getPieceLocs (setColor n11 1)
let wKings = getKingLocs (setColor n11 1)
let bAll = getPieceLocs (setColor n11 (-1))

fmap (`closestToKing` bAll) wKings

let curr = `closestToKing` []

let ctok = fmap curr wKings

module Main where

import Lib

main :: IO ()
main = someFunc
