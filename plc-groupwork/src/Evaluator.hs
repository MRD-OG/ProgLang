module Evaluator (
    runProgram
) where

import Debug.Trace (trace)

import DataStructure (
    DataEnvironment(..)
  , DataOutputBuffer(..)
  , DataVariableList(..)
  , DataDataPoint(..)
  , DataVariable(..)
  , DataValue(..))

import LangAst (
    LangAstProgram
  , LangAstInstruction(..)
  , LangAstDeclaration(..)
  , LangAstAssignment(..)
  , LangAstVariableType(..)
  , LangAstSetField(..)
  , LangAstBooleanLogic(..)
  , LangAstComparrison(..)
  , LangAstExp(..)
  , LangAstDotMethod(..)
  , LangAstAtomicValue(..)
  , LangAstBufferOperation(..))

runProgram :: LangAstProgram -> DataEnvironment -> DataEnvironment
runProgram instructions environment = executeInstructions instructions environment
  where
    extractOutputBuffer :: DataEnvironment -> String
    extractOutputBuffer (Environment _ _ (OutputBuffer outputBuffer)) = outputBuffer


executeInstructions :: LangAstProgram -> DataEnvironment -> DataEnvironment
executeInstructions (i:is) env = executeInstructions is (executeInstruction i env)
executeInstructions [] env = env 

executeInstruction :: LangAstInstruction -> DataEnvironment -> DataEnvironment
executeInstruction (Declaration decl) env = handleDeclaration decl env -- pass new variables to next instruction
-- IF : do not pass new variables to outer scope

executeInstruction _ env = env

handleDeclaration :: LangAstDeclaration -> DataEnvironment -> DataEnvironment
handleDeclaration (NodeSetDeclaration name fields labels) env = trace (show fields) env