module Evaluator (
    runProgram
) where

import Debug.Trace (trace)

import ExpressionEvaluator (
    evaluateObjectAttribute
  , evaluateBooleanCondition
  , evaluateComparrison
  , evaluateExpression
  , findMatchingNodesetIndex
  , findMatchingRelationsetIndex)

import DataStructure (
    DataEnvironment(..)
  , DataOutputBuffer(..)
  , DataVariableList(..)
  , DataDataPoint(..)
  , DataVariable(..)
  , DataType(..)
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

import N4jAst (
    N4jAstGraphData(..)
  , N4jAstDataComponent(..)
  , N4jAstGraphNodeHeader(..)
  , N4jAstHeaderField(..)
  , N4jAstHeaderFieldType(..)
  , N4jAstRelation(..)
  , N4jAstValue(..)
  , N4jAstNodeData(..))

runProgram :: LangAstProgram -> DataEnvironment -> DataEnvironment
runProgram instructions environment = executeInstructions instructions environment
  where
    extractOutputBuffer :: DataEnvironment -> String
    extractOutputBuffer (Environment _ _ (OutputBuffer outputBuffer)) = outputBuffer

executeInstructions :: LangAstProgram -> DataEnvironment -> DataEnvironment
executeInstructions (i:is) env = executeInstructions is (executeInstruction i env)
executeInstructions [] env = env 

executeInstruction :: LangAstInstruction -> DataEnvironment -> DataEnvironment
executeInstruction (Declaration decl) env = handleDeclaration decl env
executeInstruction (Assignment expression booleanExpression) env = handleAssignment expression booleanExpression env

executeInstruction _ env = env

handleDeclaration :: LangAstDeclaration -> DataEnvironment -> DataEnvironment
handleDeclaration (NodeSetDeclaration name declarationFields labels) (Environment dataComponents (VariableList varList) outputBuffer) =
  case findMatchingNodesetIndex dataComponents declarationFields labels of
    -1 -> error ("No matching field set for nodeset declaration " ++ name)
    index -> Environment dataComponents (VariableList ((NodesetVar name index) : varList)) outputBuffer

handleDeclaration (RelationSetDeclaration name declarationFields) (Environment dataComponents (VariableList varList) outputBuffer) =
  case findMatchingRelationsetIndex dataComponents declarationFields of
    -1 -> error ("No matching field set for nodeset declaration " ++ name)
    index -> Environment dataComponents (VariableList ((RelationsetVar name index) : varList)) outputBuffer

handleDeclaration (ValueDeclaration LangAst.BooleanType name booleanExpression) (Environment dataComponents (VariableList varList) outputBuffer) = 
  case evaluateBooleanCondition (Environment dataComponents (VariableList varList) outputBuffer) booleanExpression of
    Nothing -> error ("Could not evaluate expression: " ++ show (booleanExpression))
    Just (Val (BoolValue b)) -> (Environment dataComponents (VariableList ((BoolVar name b) : varList)) outputBuffer)
    Just (Var (ObjectAttribute name attr (setIndex, dataIndex))) -> (Environment dataComponents (VariableList ((BoolVar name b) : varList)) outputBuffer)
      where
        b = case evaluateObjectAttribute (Environment dataComponents (VariableList varList) outputBuffer) (ObjectAttribute name attr (setIndex, dataIndex)) of
          Just (Val (BoolValue b1)) -> b1
          _ -> error ("Mismatch type for integer declaration")

handleDeclaration (ValueDeclaration LangAst.IntegerType name booleanExpression) (Environment dataComponents (VariableList varList) outputBuffer) = 
  case evaluateBooleanCondition (Environment dataComponents (VariableList varList) outputBuffer) booleanExpression of
    Nothing -> error ("Could not evaluate expression: " ++ show (booleanExpression))
    Just (Val (IntValue i)) -> (Environment dataComponents (VariableList ((IntVar name i) : varList)) outputBuffer)
    Just (Var (ObjectAttribute name attr (setIndex, dataIndex))) -> (Environment dataComponents (VariableList ((IntVar name i) : varList)) outputBuffer)
      where
        i = case evaluateObjectAttribute (Environment dataComponents (VariableList varList) outputBuffer) (ObjectAttribute name attr (setIndex, dataIndex)) of
          Just (Val (IntValue i1)) -> i1
          _ -> error ("Mismatch type for integer declaration")

handleDeclaration (ValueDeclaration LangAst.StringType name booleanExpression) (Environment dataComponents (VariableList varList) outputBuffer) = 
  case evaluateBooleanCondition (Environment dataComponents (VariableList varList) outputBuffer) booleanExpression of
    Nothing -> error ("Could not evaluate expression: " ++ show (booleanExpression))
    Just (Val (DataStructure.StringValue s)) -> (Environment dataComponents (VariableList ((StrVar name s) : varList)) outputBuffer)
    Just (Var (ObjectAttribute name attr (setIndex, dataIndex))) -> (Environment dataComponents (VariableList ((StrVar name s) : varList)) outputBuffer)
      where
        s = case evaluateObjectAttribute (Environment dataComponents (VariableList varList) outputBuffer) (ObjectAttribute name attr (setIndex, dataIndex)) of
          Just (Val (DataStructure.StringValue s1)) -> s1
          _ -> error ("Mismatch type for integer declaration")

handleDeclaration (ValueDeclaration LangAst.NodeType name booleanExpression) (Environment dataComponents (VariableList varList) outputBuffer) = 
  case evaluateBooleanCondition (Environment dataComponents (VariableList varList) outputBuffer) booleanExpression of
    Nothing -> error ("Could not evaluate expression: " ++ show (booleanExpression))
    Just (Val (NodeValue indices)) -> (Environment dataComponents (VariableList ((NodeValueDec name indices) : varList)) outputBuffer)
    n -> error ("Invalid type assignment for Node: " ++ show n) 

handleDeclaration (ValueDeclaration LangAst.RelationType name booleanExpression) (Environment dataComponents (VariableList varList) outputBuffer) = 
  case evaluateBooleanCondition (Environment dataComponents (VariableList varList) outputBuffer) booleanExpression of
    Nothing -> error ("Could not evaluate expression: " ++ show (booleanExpression))
    Just (Var (RelationVar name indices)) -> (Environment dataComponents (VariableList ((RelationVar name indices) : varList)) outputBuffer)

handleAssignment :: LangAstExp -> LangAstBooleanLogic -> DataEnvironment -> DataEnvironment
handleAssignment expression booleanExpression env