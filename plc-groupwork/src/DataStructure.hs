module DataStructure (
    createDataEnvironment
  , DataEnvironment(..)
  , DataOutputBuffer(..)
  , DataVariableList(..)
  , DataDataPoint(..)
  , DataVariable(..)
  , DataType(..)
  , DataValue(..)
) where


import N4jAst (N4jAstGraphData(..), N4jAstDataComponent(..))

data DataEnvironment =
    Environment [N4jAstDataComponent] DataVariableList DataOutputBuffer
  deriving (Eq, Show)

newtype DataOutputBuffer =
  OutputBuffer String
  deriving (Eq, Show)

data DataVariableList =
    VariableList [DataVariable]
  deriving (Eq, Show)

data DataDataPoint = 
    Var DataVariable
  | Val DataValue
  deriving (Eq, Show)

data DataVariable =
    NodesetVar String Int --
  | RelationsetVar String Int --
  | NodeVar String (Int, Int) --
  | ObjectAttribute String String (Int, Int) -- name, attribute field name, (nodeset id, node data id)
  | RelationVar String (Int, Int)
  | HeaderFieldVar String DataType
  | NodeFieldVar String DataType
  | RelationFieldVar String DataType
  | IntVar String Int
  | StrVar String String
  | BoolVar String Bool
  | NodeValueDec String (Int, Int)
  | RelationValueDec String (Int, Int)
  deriving (Eq, Show)

data DataType =
    IntegerType
  | StringType
  | BooleanType
  | LabelType
  | TypeType
  deriving (Eq, Show)

data DataValue =
    IntValue Int
  | StringValue String
  | BoolValue Bool
  | IdentifierValue String
  | NodeValue (Int, Int)
  deriving (Eq, Show)

  
createDataEnvironment :: N4jAstGraphData -> DataEnvironment
createDataEnvironment (GraphData g) = Environment g (VariableList []) (OutputBuffer "")