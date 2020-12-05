module Main where

import Lib
import Tree

main :: IO ()
main = do
    putStrLn "Unbalanced Tree"
    printName
    putStrLn "Balanced Tree"
    printBalancedName

printName :: IO ()
printName = do
    print $ mkTree "EVANWILBUR"

printBalancedName :: IO ()
printBalancedName = do
    print $ balanceTree $ mkTree "EVANWILBUR"
