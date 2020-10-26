-- TODO: printing

{-# LANGUAGE ParallelListComp, PatternGuards #-}
module Main where

import System.Environment (getArgs)

-- Choose only one...
import Parser
import Text.Parsec

import Printer

import Debug.Trace

--------------------------------------------------------------------------------
-- Top-level wrapper code.

main :: IO ()
main = go . concat =<< mapM readFile =<< getArgs

go :: String -> IO ()
go s = putStrLn "Your implementation starts here!"
