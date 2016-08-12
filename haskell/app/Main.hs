module Main where

import Report

import System.Environment

main :: IO ()
main = getArgs >>= commandTree

commandTree :: [String] -> IO ()
commandTree ("exec":args) = cmdReport Exec args
commandTree ("test":args) = cmdReport Test args
commandTree args          = cmdReport Exec args
