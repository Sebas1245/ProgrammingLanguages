module Main where

import Lib

main :: IO ()
main = someFunc

showDemo a b c = a $ b $ c (c * c)
