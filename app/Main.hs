module Main where

import System.IO
import System.Environment
import Lib

main = do
    path <- head <$> getArgs
    src <- readFile path
    expr <- parse src
    runRuby emptyREnv $ eval expr


