module DataStructure (
    createDataEnvironment
  , DataEnvironment(..)
  , DataOutputBuffer(..)
  , DataVariableList(..)
  , DataDataPoint(..)
  , DataVariable(..)
  , DataValue(..)
) where


import N4jAst (N4jAstGraphData(..), N4jAstDataComponent(..))

data DataEnvironment =
    Environment [N4jAstDataComponent] DataVariableList DataOutputBuffer
  deriving (Eq, Show)

newtype DataOutputBuffer =
  OutputBuffer String
  deriving (Eq, Show)

newtype DataVariableList =
    VariableList [DataVariable]
  deriving (Eq, Show)

data DataDataPoint = 
    Var DataVariable
  | Val DataValue
  deriving (Eq, Show)

data DataVariable =
    NodesetVar String Int
  | RelationsetVar String Int
  | NodeVar String (Int, Int)
  | NodeAttribute String String (Int, Int)
  | RelationVar String (Int, Int)
  | IntVar String Int
  | StrVar String String
  | BoolVar String Bool
  deriving (Eq, Show)

data DataValue =
    IntValue Int
  | StringValue String
  | BoolValue Bool
  deriving (Eq, Show)

  
createDataEnvironment :: N4jAstGraphData -> DataEnvironment
createDataEnvironment (GraphData g) = Environment g (VariableList []) (OutputBuffer "")