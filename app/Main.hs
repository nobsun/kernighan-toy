module Main where

import Toysim
import System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= runToy . head
