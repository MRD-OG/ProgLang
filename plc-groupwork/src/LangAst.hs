module LangAst (
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
  , LangAstBufferOperation(..)
) where

type LangAstProgram = [LangAstInstruction]

data LangAstInstruction = 
    Declaration LangAstDeclaration
  | Assignment LangAstAssignment
  | BufferOperation LangAstBufferOperation
  | Import String
  deriving (Eq, Show)

data LangAstDeclaration =
    ValueDeclaration LangAstVariableType String LangAstBooleanLogic
  | NodeSetDeclaration String [LangAstSetField] Bool
  | RelationSetDeclaration String [LangAstSetField]
  deriving (Eq, Show)

data LangAstAssignment =
    ValueAssignment LangAstExp LangAstBooleanLogic
  deriving (Eq, Show)

data LangAstVariableType =
    IntegerType
  | StringType
  | BooleanType
  | NodeType
  | RelationType
  deriving (Eq, Show)

data LangAstSetField =
    StringField String
  | IntegerField String
  | BooleanField String
  deriving (Eq, Show)

data LangAstBooleanLogic =
    And LangAstBooleanLogic LangAstBooleanLogic
  | Or LangAstBooleanLogic LangAstBooleanLogic
  | Not LangAstBooleanLogic
  | Comparrison LangAstComparrison
  deriving (Eq, Show)

data LangAstComparrison =
    E LangAstComparrison LangAstComparrison
  | NE LangAstComparrison LangAstComparrison
  | GTE LangAstExp LangAstExp
  | LTE LangAstExp LangAstExp
  | GT LangAstExp LangAstExp
  | LT LangAstExp LangAstExp
  | Expression LangAstExp
  deriving (Eq, Show)

data LangAstExp =
    Subtracion LangAstExp LangAstExp
  | Addition LangAstExp LangAstExp
  | Multiplication LangAstExp LangAstExp
  | Division LangAstExp LangAstExp
  | Modulo LangAstExp LangAstExp
  | Power LangAstExp LangAstExp
  | Negative LangAstExp
  | Dot LangAstExp LangAstDotMethod
  | Variable String
  | Atomic LangAstAtomicValue
  deriving (Eq, Show)

data LangAstDotMethod =
    Attribute String
  | Method String [LangAstBooleanLogic]
  deriving (Eq, Show)

data LangAstAtomicValue =
    AtomicInt Int
  | AtomicString String
  | AtomicBool Bool
  | AtomicVariable String
  deriving (Eq, Show)

data LangAstBufferOperation =
    ClearBuffer
  | PushHeader [LangAstBooleanLogic] -- Nodeset/relationset [attributes]
  | PushData [LangAstBooleanLogic] -- node/relation [attributes]
  deriving (Eq, Show)