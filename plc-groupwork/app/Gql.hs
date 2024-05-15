module Main (main) where

import LangLexer (alexScanTokens)
import LangParser (parseLangTokens)

import DataStructure (
    createDataEnvironment)

import N4jLexer (alexScanTokens)
import N4jParser (parseN4jTokens)

import Evaluator (
    runProgram)

import LangAst (LangAstInstruction(..))

-- Import list
import System.Environment (getArgs)

main :: IO ()
main = do

  (filename:_) <- getArgs
  programPlaintext <- readFile filename

  let langTokenList = LangLexer.alexScanTokens programPlaintext
  let program = LangParser.parseLangTokens langTokenList

  print program

  case program of
    [] -> putStrLn "Program is empty!"
    ((Import dataFilename):restInstructions) -> do

      let filepath = dataFilename ++ ".n4j"
      graphDataPlaintext <- readFile filepath
      let n4jTokenList = N4jLexer.alexScanTokens graphDataPlaintext
      let n4jData = N4jParser.parseN4jTokens n4jTokenList
      -- print n4jTokenList
      -- print program

      let programOutput = runProgram restInstructions (createDataEnvironment n4jData)
      print programOutput
    _ -> putStrLn "Program must have a file import as the first instruction."
