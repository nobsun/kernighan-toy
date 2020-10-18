module Main where

import ToyMachine

main :: IO ()
main = interact (runToy sampleSource)
