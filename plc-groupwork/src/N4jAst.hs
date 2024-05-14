module N4jAst (
    N4jAstGraphData(..)
  , N4jAstDataComponent(..)
  , N4jAstGraphNodeHeader(..)
  , N4jAstHeaderField(..)
  , N4jAstHeaderFieldType(..)
  , N4jAstRelation(..)
  , N4jAstValue(..)
  , N4jAstNodeData(..)
) where

data N4jAstGraphData = 
    GraphData [N4jAstDataComponent]
  deriving (Eq, Show)

data N4jAstDataComponent = 
    NodesetConstruct N4jAstGraphNodeHeader [N4jAstNodeData]
  | RelationSetConstruct [N4jAstHeaderField] [N4jAstRelation]
  deriving (Eq, Show)

data N4jAstGraphNodeHeader =
    HeaderDeclaration [N4jAstHeaderField] Bool
  deriving (Eq, Show)

data N4jAstHeaderField =
    HeaderFieldDeclaration String N4jAstHeaderFieldType
  deriving (Eq, Show)

data N4jAstHeaderFieldType = 
    StringType 
  | IntegerType 
  | BooleanType 
  deriving (Eq, Show)

data N4jAstNodeData =
  NodeDataConstructor String [N4jAstValue] [String]
  deriving (Eq, Show)

-- START_ID [VALUES] END_ID TYPE
data N4jAstRelation =
    RelationDeclaration String [N4jAstValue] String String
  deriving (Eq, Show)

data N4jAstValue =
    StringValue String
  | IntegerValue Int
  | NegativeIntegerValue Int
  | BooleanValue Bool
  deriving (Eq, Show)