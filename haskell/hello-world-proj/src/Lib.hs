module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello world"

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns